open Jude
open Messages


module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7000
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

module Ping = struct
  open Actor

  type t = PingMsg.t [@@deriving bin_io]

  let receive {selfPid;_} = 
    function
    | PingMsg.Ping(senderPid) -> print_endline "got PING!";
      Printf.printf "from %s\n" (Pid.to_string senderPid);
      Luv.Time.sleep 1000;
      Arbiter.send senderPid (module PongMsg) (Pong selfPid);
end

let () = 
  Arbiter.init();

  let pid = Arbiter.spawn (module Ping) in
  let pid' = Pid.create ~id:(Uuidm.of_string "6d56fe54-45d6-4e69-a412-a0a2f114c7b5" |> Stdlib.Option.get |> Uuidm.to_bytes) "127.0.0.1" 7001 in (* run pong.exe first and fill here *)

  ignore(Luv.Thread.create (
      fun() ->
        Luv.Time.sleep 2000;
        Arbiter.send pid (module PingMsg) (PingMsg.Ping pid');
        ()
    ), ignore
    );
  Arbiter.run ()