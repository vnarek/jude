open Jude

module Backend = Backend.Make ((val Backend.create_config ()))

module Arbiter = Jude.Arbiter.Make (Backend)

module Heat_request = struct
  open Bin_prot.Std

  type t = Add_record of float | Compute_avg of Pid.t [@@deriving bin_io]
end

module Heat_reply = struct
  open Bin_prot.Std

  type t = float option [@@deriving bin_io]
end

let sum_arr = Array.fold_left Float.add 0.

let heat_aggregator ~size _ctx =
  let window = Array.make size 0. in
  let actual = ref 0 in
  let full = ref false in
  let open Matcher in
  case
    (module Heat_request)
    (function
      | Add_record f ->
          Logs.app (fun m -> m "%d" !actual);
          actual := (!actual + 1) mod size;
          if size - 1 == !actual then full := true;
          window.(!actual mod size) <- f
      | Compute_avg pid ->
          let msg =
            if !full then Some (sum_arr window /. Int.to_float size) else None
          in
          Logs.app (fun m -> m "%d" !actual);
          Arbiter.send pid (module Heat_reply) msg)

let get_heat ~size pid ctx =
  let self_pid = Actor.self_pid ctx in
  List.init size (fun k -> Int.to_float k +. 1.)
  |> List.iter (fun v -> Arbiter.send pid (module Heat_request) (Add_record v));
  Arbiter.send pid (module Heat_request) (Compute_avg self_pid);
  Matcher.case
    (module Heat_reply)
    (function
      | None ->
          Logs.err (fun m ->
              m "should return some";
              Arbiter.exit pid (`Normal pid);
              Arbiter.exit self_pid (`Normal self_pid))
      | Some f ->
          Logs.app (fun m -> m "result is: %f" f);
          Arbiter.exit self_pid (`Normal self_pid))

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let pid = Arbiter.spawn (heat_aggregator ~size:8) in
  let _ = Arbiter.spawn_link pid (get_heat ~size:8 pid) in
  Arbiter.run ()
