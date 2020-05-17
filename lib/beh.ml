module Supervisor (A : Arbiter.ARBITER) = struct
  module String_map = Map.Make (String)

  type recipe = { name : string; beh : Actor.beh }

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

  let supervise self_pid name beh =
    let pid = A.spawn beh in
    A.monitor self_pid pid;
    A.register name pid;
    pid

  let init_all t self_pid =
    String_map.iter
      (fun name recipe ->
        let new_pid = supervise self_pid name recipe.beh in
        Hashtbl.add t.running new_pid recipe)
      t.recipes

  let exit_all t self_pid =
    Hashtbl.iter
      (fun pid _ ->
        A.unmonitor self_pid pid;
        A.exit pid @@ `Normal pid)
      t.running

  let handle_exit t self_pid msg =
    let old_pid =
      match msg with
      | `Normal pid -> pid
      | `Error (msg, pid) ->
          Log.debug (fun m -> m "pid %s down reason %s" (Pid.to_string pid) msg);
          pid
    in
    match t.policy with
    | One_for_one ->
        let recipe = Hashtbl.find t.running old_pid in
        let new_pid = supervise self_pid recipe.name recipe.beh in
        Hashtbl.remove t.running old_pid;
        Hashtbl.replace t.running new_pid recipe
    | All_for_one ->
        exit_all t self_pid;
        init_all t self_pid

  let run t ctx =
    let self_pid = Actor.self_pid ctx in
    init_all t self_pid;

    Matcher.(react [ case (module System.Msg_exit) @@ handle_exit t self_pid ])
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
