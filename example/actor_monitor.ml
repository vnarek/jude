open Jude

module Backend = Backend.Make ((val Backend.create_config ()))

module Arbiter = Jude.Arbiter.Make (Backend)

module Ping_msg = struct
  type t = Ping of Pid.t [@@deriving bin_io]
end

module Pong_msg = struct
  type t = Pong of Pid.t [@@deriving bin_io]
end

let rec ping ~onfailure num ctx =
  let self_pid = Actor.self_pid ctx in
  if num == 1 then onfailure self_pid;
  Matcher.react
    [
      (Matcher.case (module Ping_msg) @@ function
       | Ping sender_pid ->
           Logs.app (fun m -> m "got PING!");
           Luv.Time.sleep 1000;
           Arbiter.send sender_pid (module Pong_msg) (Pong self_pid);
           Actor.become ctx (ping ~onfailure (num - 1)));
    ]

let rec pong () ctx =
  let self_pid = Actor.self_pid ctx in
  let pid =
    Arbiter.spawn
      (ping ~onfailure:(fun pid -> Arbiter.exit pid (`Normal self_pid)) 2)
  in
  Arbiter.register ~public:true "ping" pid;
  Arbiter.monitor self_pid pid;
  Arbiter.send pid (module Ping_msg) (Ping_msg.Ping self_pid);
  Matcher.(
    react
      [
        (case (module Pong_msg) @@ function
         | Pong sender_pid ->
             Logs.app (fun m -> m "got PONG!");
             Luv.Time.sleep 1000;
             Arbiter.send sender_pid (module Ping_msg) (Ping self_pid));
        (case (module System.Msg_exit) @@ function
         | _ ->
             Logs.app (fun m -> m "restarting");
             Actor.become ctx @@ pong ());
      ])

let () =
  (*Logs.Src.set_level Jude.Log.log_src (Some Debug)*)
  Logs.set_reporter (Logs.format_reporter ());
  let _ = Arbiter.spawn (pong ()) in
  Arbiter.run ()
