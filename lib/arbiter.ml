type channel_ops = End | Step of Actor.t

type 'a arbiter = {
  actors : (string, Actor.t) Hashtbl.t;
  mux : Luv.Rwlock.t;
  registry : Registry.t;
  actor_ch : channel_ops Channel.t;
}

module type ARBITER = sig
  val run : unit -> unit

  val spawn : Actor.beh -> Pid.t

  val spawn_link : Pid.t -> Actor.beh -> Pid.t

  val send : Pid.t -> 'a Binable.m -> 'a -> unit

  val link : Pid.t -> Pid.t -> unit

  val register : public:bool -> string -> Pid.t -> unit

  val unregister : string -> unit

  val get_name : string -> Pid.t option

  val resolve_name : string -> Pid.t -> unit

  val exit : Pid.t -> System.Exit_msg.t -> unit

  val monitor : Pid.t -> Pid.t -> unit

  val unmonitor : Pid.t -> Pid.t -> unit

  val become : Actor.t -> Actor.beh -> unit
end

module Make (B : Backend.B) : ARBITER = struct
  let arb =
    let mux = Luv.Rwlock.init () |> Result.unwrap "rwlock error" in
    {
      actors = Hashtbl.create 100;
      mux;
      registry = Registry.create ();
      actor_ch = Channel.create ();
    }

  let async_stop =
    Luv.Async.init (fun _ -> Luv.Loop.stop (Luv.Loop.default ()))
    |> Result.unwrap "loop stop error"

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

  let remove_instance_pid Pid.{ id; _ } =
    Luv.Rwlock.wrlock arb.mux;
    Hashtbl.remove arb.actors id;
    Luv.Rwlock.wrunlock arb.mux

  let find_instance_pid Pid.{ id; _ } = find_instance id

  let send_localy pid digest buf =
    match find_instance pid with
    | Some t ->
        Actor.receive t digest buf;
        Channel.send arb.actor_ch (Step t)
    | None -> Log.debug (fun m -> m "send_locally error: not_found")

  let become ctx beh =
    Actor.become ctx beh;
    Channel.send arb.actor_ch (Step ctx)

  type location = Local of string | Remote of (string * (string * int))

  let check_location pid =
    let addr_b = B.server_conn in
    (* create inside backend *)
    let addr_pid = Pid.address_port pid in
    let id = Pid.id pid in
    if addr_b = addr_pid then Local (Pid.id pid) else Remote (id, addr_pid)

  let send (type a) pid (m : a Binable.m) msg =
    let digest, msg_b = Binable.to_bytes m msg in
    match check_location pid with
    | Local id -> send_localy id digest msg_b
    | Remote (id, addr_port) ->
        let msg = System.Msg.Deliver_msg (id, digest, msg_b) in
        let buf = Binable.to_buffer (module System.Msg) msg in
        B.send addr_port buf

  let send_name_update diff =
    let msg = System.Msg.Name_update diff in
    let buf = Binable.to_buffer (module System.Msg) msg in
    B.send_all buf

  let register ~public name pid =
    Registry.register ~public ~local:true arb.registry name pid;
    if public then send_name_update (System.Diff.New (name, pid))

  let get_name name =
    Registry.(get_record arb.registry name |> Option.map (fun re -> re.pid))

  let unregister name =
    Registry.get_record arb.registry name
    |> Option.iter (fun _ -> Registry.unregister arb.registry name)

  let unregister_all = Registry.unregister_all arb.registry

  let resolve_name name customer =
    let module Res = System.Resolution_msg in
    Registry.on_name arb.registry name (fun pid ->
        send customer (module Res) @@ Res.Success (name, pid))

  let send_ready dest ack =
    let names = Registry.get_public_names arb.registry in
    let ready = System.Msg.Ready { source = B.server_conn; names; ack } in
    let buf = Binable.to_buffer (module System.Msg) ready in
    B.send dest buf

  let init () =
    let register_nonlocal = Registry.register ~local:false arb.registry in
    B.start
      ~on_conn:(fun buf ->
        let module Msg = System.Msg in
        let msg = Binable.from_buffer (module Msg) buf in
        match msg with
        | Msg.Syn source -> send_ready source false
        | Msg.Deliver_msg (pid, digest, msg) -> send_localy pid digest msg
        | Msg.Ready re ->
            List.iter
              (fun (n, pid) ->
                Log.debug (fun m -> m "registering name: %s" n);
                register_nonlocal n pid)
              re.names;
            if not re.ack then send_ready re.source true
        | Msg.Name_update diff -> (
            System.Diff.(
              match diff with
              | New (name, pid) -> register_nonlocal name pid
              | Delete name -> Registry.unregister arb.registry name) ))
      ~on_disc:(fun dest ->
        let ip, port = dest in
        Log.debug (fun m -> m "discovered: %s:%d" ip port);
        let msg = System.Msg.Syn B.server_conn in
        let buf = Binable.to_buffer (module System.Msg) msg in
        B.send dest buf)

  let spawn actor =
    let pid = Pid.create B.server_conn in
    let t = Actor.create pid in
    let id = Pid.id pid in
    save_instance t id;
    (* Lepší jako zpráva actoru arbiter *)
    Actor.init actor t;
    pid

  let link a b =
    Option.bind (find_instance_pid a) (fun a ->
        find_instance_pid b |> Option.map (fun b -> (a, b)))
    |> Option.iter (fun (a, b) ->
           Actor.link a (Actor.self_pid b);
           Actor.link b (Actor.self_pid a))

  let monitor a b =
    find_instance_pid b |> Option.iter (fun b -> Actor.monitor a b)

  let unmonitor a b =
    find_instance_pid b |> Option.iter (fun b -> Actor.unmonitor a b)

  let spawn_link a actor =
    let pid = spawn actor in
    link a pid;
    pid

  let rec exit pid a =
    let o = find_instance_pid pid in
    Option.iter (monitors_on_exit a) o;
    Option.iter (links_on_exit a) o;
    if count_instance () = 0 then Channel.send arb.actor_ch End

  and monitors_on_exit a actor =
    Actor.monitor_iter (fun p -> send p (module System.Exit_msg) a) actor

  and links_on_exit a actor =
    let pid = Actor.self_pid actor in
    if Actor.has_flag actor `Trap_exit then (
      Log.debug (fun m -> m "trapped exit pid: %s" (Pid.to_string pid));
      send pid (module System.Exit_msg) a )
    else (
      Log.debug (fun m -> m "exiting pid: %s" (Pid.to_string pid));
      remove_instance_pid pid;
      (* Maybe introduce statuses like Deleting/Active etc.*)
      Actor.link_iter (fun p -> exit p a) actor;
      unregister_all pid )

  let monitors_on_signal a actor =
    Actor.monitor_iter (fun p -> send p (module System.Signal_msg) a) actor

  let signal pid msg =
    let o = find_instance_pid pid in
    Option.iter (monitors_on_signal msg) o

  let rec actor_loop () =
    match Channel.recv arb.actor_ch with
    | Some End -> Luv.Async.send async_stop |> ignore
    | Some (Step t) ->
        ( try Actor.step t
          with e ->
            let self_pid = Actor.self_pid t in
            let err = `Exception (Printexc.to_string e, self_pid) in
            signal self_pid err );
        actor_loop ()
    | None -> ()

  let run () =
    init ();
    let t = Luv.Thread.create actor_loop |> Result.unwrap "thread create" in
    Luv.Loop.run () |> ignore;
    Luv.Thread.join t |> ignore
end
