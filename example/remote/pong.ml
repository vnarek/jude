open Jude
open Messages

module Backend =
  Backend.Make
    ((val Backend.create_config ~server:{ ip = "127.0.0.1"; port = 7001 } ()))

module Arbiter = Jude.Arbiter.Make (Backend)

let pong () ctx =
  let self_pid = Actor.self_pid ctx in
  Matcher.react
    [
      (Matcher.case (module Ping_msg) @@ function
       | Ping (sender_pid, num) ->
           Arbiter.register ~public:true "dumbledore" sender_pid;
           Logs.app (fun m -> m "got PONG! ack number:%d" num);
           Luv.Time.sleep 1;
           Arbiter.send sender_pid (module Pong_msg)
           @@ Pong (self_pid, 100 + num));
    ]

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let pid = Arbiter.spawn (pong ()) in
  Arbiter.register ~public:true "pong" pid;
  Arbiter.run ()
