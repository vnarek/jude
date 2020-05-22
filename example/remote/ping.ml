open Jude
open Messages

module Backend = Backend.Make ((val Backend.create_config ()))

module Arbiter = Arbiter.Make (Backend)
module Resolver = Beh.Resolver (Arbiter)

let ping () =
  Resolver.resolve "pong" (fun pong_pid ctx ->
      (*Luv.Time.sleep 1000;*)
      let self_pid = Actor.self_pid ctx in
      Arbiter.send pong_pid (module Ping_msg) @@ Ping (self_pid, 0);
      Matcher.react
        [
          (Matcher.case (module Pong_msg) @@ function
           | Pong (sender_pid, num) ->
               Logs.app (fun m -> m "got PING! ack number: %d" num);
               Luv.Time.sleep 1000;
               Arbiter.send sender_pid (module Ping_msg)
               @@ Ping (self_pid, num - 99));
        ])

let dummy () _ = Matcher.sink

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());

  let _ = ping () in
  let pid = Arbiter.spawn @@ dummy () in
  Arbiter.register ~public:false "dummy" pid;
  Arbiter.run ()
