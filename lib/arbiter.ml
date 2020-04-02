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

let init () =
  Backend.listen arb.server 
    (fun conn buf ->
       match System.msg_from_buffer buf with
       | System.Msg.Syn -> 
         let buf = System.msg_to_buffer System.Msg.Ready (* Actor should wait until gets ready *) in
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

let spawn actor name = 
  let pid = Pid.create name in
  let instance = Actor.create name actor in
  register instance name; (* Lepší jako zpráva actoru arbiter *)
  pid
