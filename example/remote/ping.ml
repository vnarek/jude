open Jude
open Messages


module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7000
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

module Ping = struct
  include Messages.PingMsg

  let receive Actor.{selfPid;_} = 
    function
    | PingMsg.Ping(senderPid) -> print_endline "got PING!";
      Printf.printf "from %s\n" (Pid.to_string senderPid);
      Luv.Time.sleep 1000;
      Arbiter.send senderPid (module PongMsg) (Pong selfPid);
end

let () = 
  Arbiter.init();

  let pid = Arbiter.spawn (module Ping) in
  let pid' = Pid.create ~id:(Uuidm.of_string "1202c239-7e58-4e24-8886-bef474004890" |> Stdlib.Option.get |> Uuidm.to_bytes) "127.0.0.1" 7001 in (* run pong.exe first and fill here *)

  Arbiter.send pid (module PingMsg) (PingMsg.Ping pid');
  Arbiter.run ()