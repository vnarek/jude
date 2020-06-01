module Supervisor (A : Arbiter.ARBITER) = struct
  module String_map = Map.Make (String)

  type on_error = Continue | Abort

  type recipe = {
    name : string;
    public : bool;
    beh : Actor.beh;
    on_error : on_error;
  }

  type policy = One_for_one | All_for_one

  type t = {
    policy : policy;
    recipes : recipe String_map.t;
    running : (Pid.t, recipe) Hashtbl.t;
  }

  let create ?(policy = One_for_one) recipes =
    let recipes =
      List.fold_left
        (fun acc r -> String_map.add r.name r acc)
        String_map.empty recipes
    in
    { policy; recipes; running = Hashtbl.create 20 }

  let supervise self_pid name re =
    let pid = A.spawn re.beh in
    A.monitor self_pid pid;
    A.register ~public:re.public name pid;
    pid

  let init_all t self_pid =
    String_map.iter
      (fun name re ->
        let new_pid = supervise self_pid name re in
        Hashtbl.add t.running new_pid re)
      t.recipes

  let exit_all t self_pid =
    Hashtbl.iter
      (fun pid _ ->
        A.unmonitor self_pid pid;
        A.exit pid @@ `Normal pid)
      t.running

  let handle_policy t self_pid old_pid =
    match t.policy with
    | One_for_one ->
        let re = Hashtbl.find t.running old_pid in
        let new_pid = supervise self_pid re.name re in
        Hashtbl.remove t.running old_pid;
        Hashtbl.replace t.running new_pid re
    | All_for_one ->
        exit_all t self_pid;
        init_all t self_pid

  let handle_exit t self_pid msg =
    let old_pid =
      match msg with
      | `Normal pid -> pid
      | `Error (msg, pid) ->
          Log.debug (fun m -> m "pid %s down reason %s" (Pid.to_string pid) msg);
          pid
    in
    handle_policy t self_pid old_pid

  let handle_signal t self_pid msg =
    let old_pid =
      match msg with
      | `Exception (msg, pid) ->
          Log.debug (fun m -> m "pid %s down reason %s" (Pid.to_string pid) msg);
          pid
    in
    Hashtbl.find_opt t.running self_pid
    |> Option.iter (fun r ->
           match r.on_error with
           | Continue -> ()
           | Abort -> handle_policy t self_pid old_pid)

  let run t ctx =
    let self_pid = Actor.self_pid ctx in
    init_all t self_pid;

    Matcher.(
      react
        [
          case (module System.Exit_msg) @@ handle_exit t self_pid;
          case (module System.Signal_msg) @@ handle_signal t self_pid;
        ])
end

module Resolver (A : Arbiter.ARBITER) = struct
  let resolve name fn =
    ( A.spawn (fun ctx ->
          (* TODO: Maybe multiple name resolving? *)
          let self_pid = Actor.self_pid ctx in
          A.resolve_name name self_pid;

          Matcher.(
            react
              [
                case
                  (module System.Resolution_msg)
                  (function
                    | System.Resolution_msg.Success (_, pid) ->
                        ignore (A.spawn (fn pid) : Pid.t);
                        A.exit self_pid @@ `Normal self_pid);
              ]))
      : Pid.t )
    |> ignore
end
