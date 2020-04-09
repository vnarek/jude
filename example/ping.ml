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
  open Actor
  type t = PingMsg.t [@@deriving bin_io]

  let receive {selfPid;_} = function
    | PingMsg.Ping(senderPid) -> print_endline "got PING!";
      Luv.Time.sleep 1000;
      Arbiter.send senderPid (module PongMsg) (Pong selfPid)

end

module Pong = struct
  open Actor
  type t = PongMsg.t  [@@deriving bin_io]

  let receive {selfPid;_} = function
    | PongMsg.Pong(senderPid) -> print_endline "got PONG!";
      Luv.Time.sleep 1000;
      Arbiter.send senderPid (module PingMsg) (Ping selfPid)
end

let () = 
  Arbiter.init();

  let pid = Arbiter.spawn (module Ping) "ping" in
  let pid' = Arbiter.spawn (module Pong) "pong" in

  ignore(Luv.Thread.create (
      fun() ->
        Luv.Time.sleep 2000;
        Arbiter.send pid (module PingMsg) (PingMsg.Ping pid');
    ), ignore
    );
  Arbiter.run ()