open Jude
open Messages

module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7001
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

let pong () ctx = 
  let selfPid = Actor.selfPid ctx in
  Matcher.react [
    Matcher.case (module PingMsg) @@ function
    | Ping senderPid -> 
      Logs.app (fun m -> m "got PONG!");
      Luv.Time.sleep 1000;
      Arbiter.send senderPid (module PongMsg) (Pong selfPid)
  ]

let () = 
  Logs.Src.set_level Jude.Log.log_src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  Arbiter.init();
  let pid = Arbiter.spawn (pong ()) in
  Arbiter.register "pong" pid;
  Arbiter.run ()