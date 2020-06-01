open Jude

module Backend = Backend.Make ((val Backend.create_config ()))

module Arbiter = Jude.Arbiter.Make (Backend)
module Supervisor = Jude.Beh.Supervisor (Arbiter)

module Ping_msg = struct
  type t = Ping of Pid.t [@@deriving bin_io]
end

module Pong_msg = struct
  type t = Pong [@@deriving bin_io]
end

let rec ping num ctx =
  if num == 0 then failwith "i hate 0s"
  else
    Matcher.react
      [
        (Matcher.case (module Ping_msg) @@ function
         | Ping pid ->
             Logs.app (fun m -> m "got PING!");
             Luv.Time.sleep 200;
             Arbiter.send pid (module Pong_msg) Pong;
             Actor.become ctx @@ ping @@ (num - 1));
      ]

let pong () ctx =
  let ping_pid = Arbiter.get_name "ping" |> Option.get in
  let self_pid = Actor.self_pid ctx in
  Arbiter.send ping_pid (module Ping_msg) @@ Ping self_pid;
  Matcher.react
    [
      (Matcher.case (module Pong_msg) @@ function
       | Pong ->
           Logs.app (fun m -> m "got PONG!");
           Luv.Time.sleep 200;
           Arbiter.send ping_pid (module Ping_msg) @@ Ping self_pid);
    ]

let () =
  (*Logs.Src.set_level Jude.Log.log_src (Some Debug);*)
  Logs.set_reporter @@ Logs.format_reporter ();
  let t =
    Supervisor.create ~policy:Supervisor.All_for_one
      [
        { name = "ping"; public = true; beh = ping 2; on_error = Abort };
        { name = "pong"; public = true; beh = pong (); on_error = Abort };
      ]
  in
  let _ = Arbiter.spawn @@ Supervisor.run t in
  Arbiter.run ()
