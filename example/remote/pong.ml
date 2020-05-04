open Jude
open Messages

module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7001
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

module Pong = struct
  include PongMsg

  let receive Actor.{selfPid;_} = Matcher.react [
      Matcher.case (module PingMsg) @@ function
      | Ping senderPid -> 
        Logs.app (fun m -> m "got PING!");
        Luv.Time.sleep 1000;
        Arbiter.send senderPid (module PongMsg) (Pong selfPid)
    ]
end

let () = 
  Arbiter.init();
  let pid = Arbiter.spawn (module Pong) in

  Pid.to_string pid |> print_endline;
  Arbiter.run ()