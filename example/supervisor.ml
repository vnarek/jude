open Jude

module Backend = Jude.Backend.Make (struct
  let server_ip = "127.0.0.1"

  let server_port = 7000
end)

module Arbiter = Jude.Arbiter.Make (Backend)
module Supervisor = Jude.Beh.Supervisor (Arbiter)

module PingMsg = struct
  type t = Ping of Pid.t [@@deriving bin_io]
end

module PongMsg = struct
  type t = Pong [@@deriving bin_io]
end

let rec ping num ctx =
  if num == 0 then failwith "i hate 0s"
  else
    Matcher.react
      [
        (Matcher.case (module PingMsg) @@ function
         | Ping pid ->
             Logs.app (fun m -> m "got PING!");
             Luv.Time.sleep 200;
             Arbiter.send pid (module PongMsg) Pong;
             Actor.become ctx @@ ping @@ (num - 1));
      ]

let pong () ctx =
  let ping_pid = Arbiter.get_name "ping" |> Option.get in
  let self_pid = Actor.self_pid ctx in
  Arbiter.send ping_pid (module PingMsg) @@ Ping self_pid;
  Matcher.react
    [
      (Matcher.case (module PongMsg) @@ function
       | Pong ->
           Logs.app (fun m -> m "got PONG!");
           Luv.Time.sleep 200;
           Arbiter.send ping_pid (module PingMsg) @@ Ping self_pid);
    ]

let () =
  (*Logs.Src.set_level Jude.Log.log_src (Some Debug);*)
  Logs.set_reporter @@ Logs.format_reporter ();
  Arbiter.init ();
  let t =
    Supervisor.create ~policy:Supervisor.All_for_one
      [ { name = "ping"; beh = ping 2 }; { name = "pong"; beh = pong () } ]
  in
  let _ = Arbiter.spawn @@ Supervisor.run t in
  Arbiter.run ()
