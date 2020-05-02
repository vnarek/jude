type 'a arbiter = {
  actors : (string, (module Actor.INSTANCE)) Hashtbl.t;
  mux : Luv.Rwlock.t;
  actor_ch : (module Actor.INSTANCE) Channel.t;
}

module type ARBITER = sig
  val init : unit -> unit
  val run : unit -> unit
  val spawn : 'a Actor.def -> Pid.t
  val send : Pid.t -> 'a Binable.m -> 'a -> unit
end

module Make(B: Backend.B): ARBITER = struct
  let arb =
    let mux = Luv.Rwlock.init () |> Result.get_ok in
    {
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
    let instance = Hashtbl.find_opt arb.actors name in
    Luv.Rwlock.rdunlock arb.mux;
    instance

  let rec actor_loop () =
    Channel.recv arb.actor_ch
    |> Option.iter (fun (module I: Actor.INSTANCE) -> I.step());
    actor_loop()

  let send_localy pid msg_s = 
    find_instance pid
    |> Option.iter (fun (module I: Actor.INSTANCE) ->
        Luv.Buffer.from_bytes msg_s
        |> I.receive;
        Channel.send arb.actor_ch (module I)
      )

  let init () =
    B.listen
      (fun conn buf ->
         let buf = Luv.Buffer.to_bytes buf in
         match Binable.from_bytes (module System.Msg) buf |> Result.get_ok (*UPRAVIT!*) with
         | Syn -> 
           let (_, buf') = Binable.to_bytes (module System.Msg) System.Msg.Ready in
           let buf = Luv.Buffer.from_bytes buf' in(* Actor should wait until gets ready *)
           Luv.Stream.write conn [buf] (fun _ _ -> ());
         | ToActor (pid, msg) -> 
           print_endline "autor:";
           print_endline pid;
           print_endline "msg:";
           print_endline (Bytes.to_string msg);
           send_localy pid msg;
         | _ -> print_endline "nononoe";
      )

  let run () = 
    let _ = Luv.Thread.create (actor_loop) (* Join this later *) in
    Luv.Loop.run () |> ignore

  let spawn  actor = 
    let pid = Pid.create B.server_ip B.server_port in
    let instance = Actor.create pid actor in
    let id = Pid.id pid in
    register instance id; (* Lepší jako zpráva actoru arbiter *)
    pid

  type location = Local of string | Remote of (string * (string * int))

  let check_location pid =
    let addr_b = B.server_ip, B.server_port in
    let addr_pid = Pid.address_port pid in
    let id = Pid.id pid in
    if addr_b = addr_pid then
      Local (Pid.id pid)
    else
      Remote (id, addr_pid)

  let send (type a) pid (m: a Binable.m) msg =
    let (_, msg_b) = Binable.to_bytes m msg in
    match check_location pid with
    |Local id ->
      send_localy id msg_b
    |Remote (id, addr_port) ->
      let msg =  System.Msg.ToActor (id, msg_b) in
      let (_, buf) = Binable.to_bytes (module System.Msg) msg in 
      let buf = Luv.Buffer.from_bytes buf in
      B.send addr_port buf;
      print_endline "sended";
end