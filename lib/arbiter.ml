type channel_ops = End | Step of Actor.t

type 'a arbiter = {
  actors : (string, Actor.t) Hashtbl.t;
  mux : Luv.Rwlock.t;
  registry: Registry.t;
  actor_ch : channel_ops Channel.t;
}

type actor = Actor.t -> Matcher.t

module type ARBITER = sig
  val init : unit -> unit
  val run : unit -> unit
  val spawn : actor -> Pid.t
  val spawn_link: Pid.t -> actor -> Pid.t
  val send : Pid.t -> 'a Binable.m -> 'a -> unit
  val link: Pid.t -> Pid.t -> unit
  val register: string -> Pid.t -> unit
  val unregister: string -> unit
  val get_name: string -> Pid.t option
  val resolve_name: string -> Pid.t -> unit
  val exit: Pid.t -> System.Msg_exit.t -> unit
  val unmonitor: Pid.t -> Pid.t -> unit
  val monitor: Pid.t -> Pid.t -> unit
  val become: Actor.t -> Actor.beh -> unit
end

module Make_log(B: Backend.B)(Log: Logs.LOG): ARBITER = struct
  let arb =
    let mux = Luv.Rwlock.init () |> Result.get_ok in
    {
      actors = Hashtbl.create 100;
      mux = mux;
      registry = Registry.create ();
      actor_ch = Channel.create ();
    }

  let async_stop = Luv.Async.init 
      (fun _ ->  Luv.Loop.stop (Luv.Loop.default ()))
                   |> Result.get_ok

  let save_instance instance name =
    Luv.Rwlock.wrlock arb.mux;
    Hashtbl.replace arb.actors name instance;
    Luv.Rwlock.wrunlock arb.mux

  let count_instance () =
    Luv.Rwlock.rdlock arb.mux;
    let num = Hashtbl.length arb.actors in
    Luv.Rwlock.rdunlock arb.mux;
    num

  let find_instance name =
    Luv.Rwlock.rdlock arb.mux;
    let instance = Hashtbl.find_opt arb.actors name in
    Luv.Rwlock.rdunlock arb.mux;
    instance

  let remove_instance_pid Pid.{id; _} =
    Luv.Rwlock.wrlock arb.mux;
    Hashtbl.remove arb.actors id;
    Luv.Rwlock.wrunlock arb.mux

  let find_instance_pid Pid.{id;_} = find_instance id

  let send_localy pid digest buf = 
    match find_instance pid with
    |Some t ->
      Actor.receive t digest buf;
      Channel.send arb.actor_ch (Step t)
    |None ->
      Log.debug (fun m -> m "send_locally error: not_found")

  let become ctx beh = 
    Actor.become ctx beh;
    Channel.send arb.actor_ch (Step ctx)

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
      B.send addr_port buf

  let register = Registry.register ~local:true arb.registry

  let unregister = Registry.unregister arb.registry

  let unregister_all = Registry.unregister_all arb.registry

  let get_name = Registry.get_name arb.registry

  let resolve_name name customer = 
    Registry.on_name arb.registry name (fun pid ->
        send customer (module System.Resolution_msg) @@ Success (name, pid)
      )

  let send_ready dest ack = 
    let names = Registry.get_public_names arb.registry in
    let ready = System.Msg.Ready ({
        source=(B.server_ip, B.server_port);
        names=names;
        ack=ack
      }) in
    let (_, buf) = Binable.to_bytes (module System.Msg) ready in
    let buf = Luv.Buffer.from_bytes buf in
    B.send dest buf

  let init () =
    B.start
      ~on_tcp:(fun _conn buf ->
          let buf = Luv.Buffer.to_bytes buf in (*TODO: Get rid of this conversion *)
          let msg = Binable.from_bytes (module System.Msg) buf |> Result.get_ok in
          match msg with
          | Syn source ->
            send_ready source false;
          | ToActor (pid, digest, msg) ->
            Log.debug (fun m -> m "send locally: %s" pid);
            send_localy pid digest msg;
          | Ready re ->
            List.iter (fun (n, pid) ->
                Log.debug (fun m -> m "registering name: %s" n);
                Registry.register ~local:false arb.registry n pid;
              ) re.names;
            if not re.ack then
              send_ready re.source true;
        )
      ~on_disc:(fun dest ->
          let ip, port = dest in
          Log.debug (fun m -> m "discovered: %s:%d" ip port);
          let msg = System.Msg.Syn (B.server_ip, B.server_port) in
          let _, b = Binable.to_bytes (module System.Msg) msg in
          let buffer = Luv.Buffer.from_bytes b in
          B.send dest buffer
        )


  let spawn actor = 
    let pid = Pid.create B.server_ip B.server_port in
    let t = Actor.create pid in
    let id = Pid.id pid in
    save_instance t id; (* Lepší jako zpráva actoru arbiter *)
    Actor.init actor t;
    pid

  let link a b =
    Option.bind (find_instance_pid a) (fun a ->
        find_instance_pid b
        |> Option.map (
          fun b -> (a, b)
        )
      )
    |> Option.iter (fun (a, b) -> 
        Actor.link a (Actor.selfPid b);
        Actor.link b (Actor.selfPid a);
      )

  let monitor a b =
    find_instance_pid b
    |> Option.iter (fun b -> 
        Actor.monitor a b;
      )

  let unmonitor a b =
    find_instance_pid b
    |> Option.iter (fun b ->
        Actor.unmonitor a b;
      )

  let spawn_link a actor =
    let pid = spawn actor in
    link a pid;
    pid

  let rec exit pid a =
    let o = find_instance_pid pid in
    Option.iter (monitors_on_exit a) o;
    Option.iter (links_on_exit a) o;
    if count_instance () = 0 then
      Channel.send arb.actor_ch End
  and monitors_on_exit a actor =
    Actor.monitor_iter (fun p -> 
        send p (module System.Msg_exit) a
      ) actor
  and links_on_exit a actor  =
    let pid = Actor.selfPid actor in
    if Actor.has_flag actor `Trap_exit then 
      begin
        Log.debug (fun m -> m "trapped exit pid: %s" (Pid.to_string pid));
        send pid (module System.Msg_exit) a
      end 
    else
      begin
        Log.debug (fun m -> m "exiting pid: %s" (Pid.to_string pid));
        remove_instance_pid pid; (* Maybe introduce statuses like Deleting/Active etc.*)
        Actor.link_iter (fun p -> exit p a) actor;
        unregister_all pid;
      end 

  let rec actor_loop () =
    match Channel.recv arb.actor_ch with
    | Some(End) -> Luv.Async.send async_stop |> ignore
    | Some(Step t) -> begin
        try 
          Actor.step t;
        with 
        | e -> 
          let err = `Error (Printexc.to_string e, Actor.selfPid t) in
          exit (Actor.selfPid t) err;
      end;
      actor_loop()
    | None -> ()

  let run () = 
    let t = Luv.Thread.create (actor_loop) |> Result.get_ok (* Join this later *) in
    (Luv.Loop.run ()) |> ignore;
    Luv.Thread.join t |> ignore
end

module Make(B: Backend.B) = Make_log(B)(Log.Log)
