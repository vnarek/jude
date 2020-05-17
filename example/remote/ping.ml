open Jude
open Messages

module Backend = Backend.Make (struct
  let server_ip = "127.0.0.1"

  let server_port = 7000
end)

module Arbiter = Arbiter.Make (Backend)
module Resolver = Beh.Resolver (Arbiter)

let ping () =
  Resolver.resolve "pong" (fun pong_pid ctx ->
      (*Luv.Time.sleep 1000;*)
      let self_pid = Actor.self_pid ctx in
      Arbiter.send pong_pid (module PingMsg) @@ Ping (self_pid, 0);
      Matcher.react
        [
          (Matcher.case (module PongMsg) @@ function
           | Pong (senderPid, num) ->
               Logs.app (fun m -> m "got PING! ack number: %d" num);
               Luv.Time.sleep 1000;
               Arbiter.send senderPid (module PingMsg)
               @@ Ping (self_pid, num - 99));
        ])

let dummy () _ = Matcher.sink

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());

  let _ = ping () in
  let pid = Arbiter.spawn @@ dummy () in
  Arbiter.register "dummy" pid;
  Arbiter.run ()
