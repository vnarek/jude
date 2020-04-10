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
  let pid' = Pid.create ~id:(Uuidm.of_string "7436b958-28b6-461e-8d60-4752a2998d0e" |> Stdlib.Option.get |> Uuidm.to_bytes) "127.0.0.1" 7001 in (* run pong.exe first and fill here *)

  ignore(Luv.Thread.create (
      fun() ->
        Luv.Time.sleep 2000;
        Arbiter.send pid (module PingMsg) (PingMsg.Ping pid');
        (*Luv.Stream.write w [Luv.Buffer.from_string "Hello mate"] (fun e _ -> Result.get_error e |> Luv.Error.strerror |> print_endline);*)
        ()
    ), ignore
    );
  Arbiter.run ()