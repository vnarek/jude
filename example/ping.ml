open Jude

module Backend = Jude.Backend.Make (struct
  let server_ip = "127.0.0.1"

  let server_port = 7000
end)

module Arbiter = Jude.Arbiter.Make (Backend)

module PingMsg = struct
  type t = Ping of Pid.t [@@deriving bin_io]
end

module PongMsg = struct
  type t = Pong of Pid.t [@@deriving bin_io]
end

let ping () ctx =
  let self_pid = Actor.self_pid ctx in
  Matcher.react
    [
      (Matcher.case (module PingMsg) @@ function
       | Ping senderPid ->
           Logs.app (fun m -> m "got PING!");
           Luv.Time.sleep 1000;
           Arbiter.send senderPid (module PongMsg) (Pong self_pid));
    ]

let pong () ctx =
  let self_pid = Actor.self_pid ctx in
  let pid = Arbiter.spawn (ping ()) in
  Arbiter.send pid (module PingMsg) (PingMsg.Ping self_pid);
  Matcher.react
    [
      (Matcher.case (module PongMsg) @@ function
       | Pong senderPid ->
           Logs.app (fun m -> m "got PONG!");
           Luv.Time.sleep 1000;
           Arbiter.send senderPid (module PingMsg) (Ping self_pid));
    ]

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let _ = Arbiter.spawn (pong ()) in
  Arbiter.run ()
