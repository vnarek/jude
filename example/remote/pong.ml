open Jude
open Messages

module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7001
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

module Pong = struct
  include PongMsg

  let receive Actor.{selfPid;_} = function
    | PongMsg.Pong(senderPid) -> print_endline "got PONG!";
      Luv.Time.sleep 200;
      Arbiter.send senderPid (module PingMsg) (Ping selfPid)
end

let () = 
  Arbiter.init();
  let pid = Arbiter.spawn (module Pong) in

  Pid.to_string pid |> print_endline;
  Arbiter.run ()