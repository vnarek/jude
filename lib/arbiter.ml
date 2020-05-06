type 'a arbiter = {
  actors : (string, Actor.t) Hashtbl.t;
  mux : Luv.Rwlock.t;
  actor_ch : Actor.t Channel.t;
}

type actor = Actor.t -> Matcher.t

module type ARBITER = sig
  val init : unit -> unit
  val run : unit -> unit
  val spawn : actor -> Pid.t
  val send : Pid.t -> 'a Binable.m -> 'a -> unit
end

module Make_log(B: Backend.B)(Log: Logs.LOG): ARBITER = struct
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
    |> Option.iter (
      fun t -> 
        Actor.step t
        |> Result.iter_error @@ fun r ->
        Log.debug (fun m -> m "receive: %s" r)
    );
    actor_loop()

  let send_localy pid digest buf = 
    match find_instance pid with
    |Some t ->
      Actor.receive t digest buf;
      Channel.send arb.actor_ch t
    |None ->
      Log.debug (fun m -> m "send_locally error: not_found\n")

  let init () =
    B.listen
      (fun conn buf ->
         let buf = Luv.Buffer.to_bytes buf in
         match Binable.from_bytes (module System.Msg) buf |> Result.get_ok (*UPRAVIT!*) with
         | Syn -> 
           let (_, buf') = Binable.to_bytes (module System.Msg) System.Msg.Ready in
           let buf = Luv.Buffer.from_bytes buf' in
           Luv.Stream.write conn [buf] (fun _ _ -> ());
         | ToActor (pid, digest, msg) ->
           Log.debug (fun m -> m "to_actor(dest:%s)" pid);
           send_localy pid digest msg;
         | _ -> Log.warn (fun m -> m "this should never happen")
      )

  let run () = 
    let _ = Luv.Thread.create (actor_loop) (* Join this later *) in
    Luv.Loop.run () |> ignore

  let spawn actor = 
    let pid = Pid.create B.server_ip B.server_port in
    let t = Actor.create pid in
    let id = Pid.id pid in
    register t id; (* Lepší jako zpráva actoru arbiter *)
    Actor.init actor t;
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
    let digest, msg_b = Binable.to_bytes m msg in
    match check_location pid with
    |Local id ->
      send_localy id digest msg_b
    |Remote (id, addr_port) ->
      let msg =  System.Msg.ToActor (id, digest, msg_b) in
      let (_, buf) = Binable.to_bytes (module System.Msg) msg in 
      let buf = Luv.Buffer.from_bytes buf in
      B.send addr_port buf;
end

module Make(B: Backend.B) = Make_log(B)(Log.Log)
