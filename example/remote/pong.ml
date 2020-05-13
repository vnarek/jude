open Jude
open Messages

module Backend = Jude.Backend.Make (struct
  let server_ip = "127.0.0.1"

  let server_port = 7001
end)

module Arbiter = Jude.Arbiter.Make (Backend)

let pong () ctx =
  let self_pid = Actor.self_pid ctx in
  Matcher.react
    [
      (Matcher.case (module PingMsg) @@ function
       | Ping (senderPid, num) ->
           Logs.app (fun m -> m "got PONG! ack number:%d" num);
           Luv.Time.sleep 1000;
           Arbiter.send senderPid (module PongMsg)
           @@ Pong (self_pid, 100 + num));
    ]

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  Arbiter.init ();
  let pid = Arbiter.spawn (pong ()) in
  Arbiter.register "pong" pid;
  Arbiter.run ()
