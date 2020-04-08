type 'a arbiter = {
  server: Luv.TCP.t;
  actors: (string, (module Actor.Instance)) Hashtbl.t;
  mux: Luv.Rwlock.t;
  actor_ch: (module Actor.Instance) Channel.t;
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
  |> Option.iter (fun (module I: Actor.Instance) -> I.step());
  actor_loop()

let send_localy pid msg_s = 
  let (module I) = find_instance pid in
  Luv.Buffer.from_string msg_s
  |> I.receive;
  Channel.send arb.actor_ch (module I)

let init () =
  Backend.listen arb.server 
    (fun conn buf ->
       match System.msg_from_buffer buf with
       | System.Msg.Syn -> 
         let buf = System.msg_to_buffer System.Msg.Ready (* Actor should wait until gets ready *) in
         Luv.Stream.write conn [buf] (fun _ _ -> ());
       | ToActor (pid, msg) -> 
         print_endline "autor:";
         print_endline pid;
         print_endline "msg:";
         print_endline msg;
         send_localy pid msg;
       | _ -> print_endline "nononoe";
    )

let run () = 
  let _ = Luv.Thread.create (actor_loop) (* Join this later *) in
  Luv.Loop.run () |> ignore

let spawn actor id = 
  let pid = Pid.create id in
  let instance = Actor.create pid actor in
  register instance id; (* Lepší jako zpráva actoru arbiter *)
  pid

type location = Local | Remote

let check_location pid =
  let addr_b = Backend.get_address_s Backend.server_address in
  let addr_pid = Pid.adress_to_string pid in
  if String.equal addr_b addr_pid then
    Local
  else
    Remote

let send (type a) pid (m: a Core_kernel.Binable.m) msg =
  let msg_s = Core_kernel.Binable.to_string m msg in
  let id = Pid.id pid in

  match check_location pid with
  |Local -> 
    print_endline "local system";
    send_localy id msg_s 
  |Remote ->
    let _ = System.msg_to_buffer (System.Msg.ToActor (id, msg_s)) in
    () (* Todo implement remote sending *)