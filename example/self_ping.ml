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

module Pong() = struct
  include PongMsg

  let receive Actor.{selfPid;_} =
    Arbiter.send selfPid (module PongMsg) (PongMsg.Pong selfPid);
    Matcher.(
      react [
        case (module PongMsg) @@ function
        | Pong senderPid ->
          Logs.app (fun m -> m "got PONG!");
          Luv.Time.sleep 1000;
          Arbiter.send senderPid (module PingMsg) (Ping selfPid)
      ]
    )
  and ping Actor.{selfPid; _} =
    Matcher.(
      react [
        case (module PingMsg) @@ function
        | Ping senderPid ->
          Logs.app (fun m -> m "got PING!");
          Luv.Time.sleep 1000;
          Arbiter.send senderPid (module PongMsg) (Pong selfPid)
      ]
    )
end

let () =
  Logs.Src.set_level Jude.Log.log_src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  Arbiter.init();
  let _ = Arbiter.spawn (module Pong()) in
  Arbiter.run ()