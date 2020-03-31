module SystemMsg = struct 
  open Bin_prot.Std

  type t =  
      Syn
    | Ready
    | ToActor of (string * string) [@@deriving bin_io]
end

let handshake_to_buffer msg = 
  Core_kernel.Binable.to_string (module SystemMsg) msg
  |> Luv.Buffer.from_string

let handshake_from_buffer buf = 
  Luv.Buffer.to_string buf
  |> Core_kernel.Binable.of_string (module SystemMsg)

module type Instance = sig
  val receive: Luv.Buffer.t -> unit
  val step: unit -> unit
end

type 'a arbiter = {
  server: Luv.TCP.t;
  actors: (string, (module Instance)) Hashtbl.t;
  mux: Luv.Rwlock.t;
  actor_ch: (module Instance) Channel.t;
}

let arb =
  let mux = Luv.Rwlock.init () |> Result.get_ok in
  {
    server = Backend.create_arbiter ();
    actors = Hashtbl.create 100;
    mux = mux;
    actor_ch = Channel.create ();
  }

let register instance name =
  Luv.Rwlock.wrlock arb.mux;
  Hashtbl.add arb.actors name instance;
  Luv.Rwlock.wrunlock arb.mux

let find_instance name =
  Luv.Rwlock.rdlock arb.mux;
  let instance = Hashtbl.find_opt arb.actors name 
                 |> Option.get in
  Luv.Rwlock.rdunlock arb.mux;
  instance

let rec actor_loop () =
  Channel.recv arb.actor_ch
  |> Option.iter (fun (module I: Instance) -> I.step());
  actor_loop()

let init () =
  Backend.listen arb.server 
    (fun conn buf ->
       match handshake_from_buffer buf with
       | SystemMsg.Syn -> 
         let buf = handshake_to_buffer SystemMsg.Ready (* Actor should wait until gets ready *) in
         Luv.Stream.write conn [buf] (fun _ _ -> ());
       | ToActor (name, msg) -> 
         print_endline "autor:";
         print_endline name;
         print_endline "msg:";
         print_endline msg;
         let (module I) = find_instance name in
         Luv.Buffer.from_string msg
         |> I.receive;
         Channel.send arb.actor_ch (module I)
       | _ -> print_endline "nononoe";
    )

let run () = 
  let _ = Luv.Thread.create (actor_loop) (* Join this later *) in
  Luv.Loop.run () |> ignore

let handshake fn client =
  let buf = handshake_to_buffer (SystemMsg.Syn) in 
  Luv.Stream.write client [buf] (fun _ _ -> ());
  Luv.Stream.read_start client begin function
    | Error `EOF ->
      Luv.Handle.close client ignore
    | Error e ->
      Printf.eprintf "Read error: %s\n" (Luv.Error.strerror e);
      Luv.Handle.close client ignore
    | Ok res -> match handshake_from_buffer res with
      | Ready -> 
        print_endline "handshake completed";
        (fn client)
      | _ -> ()
  end


module type Def = sig
  type t [@@deriving bin_io]
  val receive: t -> unit
end

type 'a def = (module Def with type t = 'a)

module type PID = sig
  type t
  val name: string
  val send: t -> unit
end
type 'a pid = (module PID with type t = 'a)



let send (type a) name client (t: a def) msg =
  let (module T) = t in
  let msg_s = Core_kernel.Binable.to_string (module T) msg in
  let buf = handshake_to_buffer (SystemMsg.ToActor (name, msg_s)) in
  Luv.Stream.write client [buf] (fun _ _ -> ())

let create_pid (type a) client name (t: a def) =
  let (module T) = t in
  let module M = struct
    type t = T.t

    let name = name
    let send msg = send name client (module T) msg
  end in
  (module M: PID with type t = a)

let receive (type a) (t: a def) mailbox buf =
  let (module T) = t in
  let msg = Luv.Buffer.to_string buf
            |> Core_kernel.Binable.of_string (module T) in
  Mailbox.push mailbox msg

let create_instance (type a) (t: a def) =
  let (module T) = t in
  let mailbox = Mailbox.create () |> Result.get_ok (*FIXME pls*)in
  let module M = struct
    let receive buf = receive (module T) mailbox buf
    let step () = 
      Mailbox.take mailbox 
      |> Option.get
      |> T.receive

  end in (module M: Instance)



let spawn actor name = 
  let client = Backend.create_client () in
  let pid = create_pid client name actor in
  let instance = create_instance actor in
  register instance name; (* Lepší jako zpráva actoru arbiter *)
  Backend.connect client (handshake ignore);
  pid
