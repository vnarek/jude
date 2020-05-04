open Jude



module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7000
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

module PingMsg = struct
  type t = Ping of Pid.t [@@deriving bin_io]
end

module PongMsg = struct
  type t = Pong of Pid.t [@@deriving bin_io]
end

module Ping = struct
  include PingMsg

  let receive Actor.{selfPid;_} = Matcher.react [
      Matcher.case (module PingMsg) @@ function
      | Ping senderPid -> 
        Logs.app (fun m -> m "got PING!");
        Luv.Time.sleep 1000;
        Arbiter.send senderPid (module PongMsg) (Pong selfPid)
    ]
end

module Pong() = struct
  include PongMsg

  let receive Actor.{selfPid;_} =
    let pid = Arbiter.spawn (module Ping) in
    Arbiter.send pid (module PingMsg) (PingMsg.Ping selfPid);
    Matcher.react [
      Matcher.case (module PongMsg) @@ function
      | Pong senderPid ->
        Logs.app (fun m -> m "got PONG!");
        Luv.Time.sleep 1000;
        Arbiter.send senderPid (module PongMsg) (Pong selfPid)
    ]
end

let () =
  Logs.Src.set_level Jude.Log.log_src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  Arbiter.init();
  let _ = Arbiter.spawn (module Pong()) in
  Arbiter.run ()