type 'a arbiter = {
  server : Luv.TCP.t;
  actors : (string, (module Actor.INSTANCE)) Hashtbl.t;
  arbiters: ((string * int), Luv.TCP.t) Hashtbl.t;
  mux : Luv.Rwlock.t;
  actor_ch : (module Actor.INSTANCE) Channel.t;
}

module type ARBITER = sig
  val find_instance : string -> (module Actor.INSTANCE)
  val init : unit -> unit
  val run : unit -> unit
  val spawn : 'a Actor.def -> Pid.t
  val send : Pid.t -> 'a Core_kernel.Binable.m -> 'a -> unit
end

module Make(B: Backend.B): ARBITER = struct
  let arb =
    let mux = Luv.Rwlock.init () |> Result.get_ok in
    {
      server = B.create_arbiter ();
      actors = Hashtbl.create 100;
      arbiters = Hashtbl.create 100;
      mux = mux;
      actor_ch = Channel.create ();
    }

  let get_arbiter pid = (* Todo lock this *)
    let addr_port = Pid.address_port pid in
    match Hashtbl.find_opt arb.arbiters addr_port with
    | Some arb -> arb
    | None ->
      let client = B.connect addr_port ignore in
      Hashtbl.add arb.arbiters addr_port client;
      client


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
    |> Option.iter (fun (module I: Actor.INSTANCE) -> I.step());
    actor_loop()

  let send_localy pid msg_s = 
    let (module I) = find_instance pid in
    Luv.Buffer.from_string msg_s
    |> I.receive;
    Channel.send arb.actor_ch (module I)

  let init () =
    B.listen arb.server 
      (fun conn buf ->
         match System.msg_from_buffer buf with
         | Syn -> 
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

  let spawn  actor = 
    let pid = Pid.create B.server_ip B.server_port in (* TODO: Maybe generate uuid inside Pid.create*)
    let instance = Actor.create pid actor in
    let id = Pid.id pid in
    register instance id; (* Lepší jako zpráva actoru arbiter *)
    pid

  type location = Local | Remote

  let check_location pid =
    let addr_b = B.server_address_s in
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
      send_localy id msg_s 
    |Remote ->
      let handle = Luv.Async.init (fun _ ->
          let buf = System.msg_to_buffer (System.Msg.ToActor (id, msg_s)) in
          let arbiter = get_arbiter pid in
          Luv.Stream.write arbiter [buf] (fun e _ -> 
              match e with 
              | Error x -> print_endline (Luv.Error.strerror x) 
              | Ok () -> print_endline "no error");
        ) |> Result.get_ok in
      ignore(Luv.Async.send handle)
end