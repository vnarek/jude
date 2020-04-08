open Jude

module PingMsg = struct
  type t = Ping of Pid.pid [@@deriving bin_io]
end

module PongMsg = struct
  type t = Pong of Pid.pid [@@deriving bin_io]
end

module Ping = struct
  open Actor
  type t = PingMsg.t [@@deriving bin_io]

  let receive {name=name} = function
    | PingMsg.Ping(x) -> print_endline "got PING!";
      Luv.Time.sleep 1000;
      let pid = Pid.create x in

      Pid.send pid (module PongMsg) (Pong name)

end

module Pong = struct
  open Actor
  type t = PongMsg.t  [@@deriving bin_io]

  let receive {name=name} = function
    | PongMsg.Pong(x) -> print_endline "got PONG!";
      Luv.Time.sleep 1000;
      let pid = Pid.create x in

      Pid.send pid (module PingMsg) (Ping name)
end

let () = 
  Arbiter.init();

  let pid = Arbiter.spawn (module Ping) "ping" in
  let pid' = Arbiter.spawn (module Pong) "pong" in

  ignore(Luv.Thread.create (
      fun() ->
        Luv.Time.sleep 2000;
        Pid.send pid (module PingMsg) (PingMsg.Ping (Pid.get_pid pid'));
    ), ignore
    );
  Arbiter.run ()