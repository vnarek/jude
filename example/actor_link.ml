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

let pong () ctx =
  Actor.set_flag ctx `Trap_exit;
  let self_pid = Actor.self_pid ctx in
  let pid =
    Arbiter.spawn_link self_pid
      (ping ~onfailure:(fun pid -> Arbiter.exit pid (`Normal self_pid)) 2)
  in
  let pid' =
    Arbiter.spawn_link self_pid (ping ~onfailure:(fun _ -> 1 / 0 |> ignore) 2)
  in
  Arbiter.send pid (module Ping_msg) (Ping_msg.Ping self_pid);
  Arbiter.send pid' (module Ping_msg) (Ping_msg.Ping self_pid);
  Matcher.react
    [
      (Matcher.case (module Pong_msg) @@ function
       | Pong sender_pid ->
           Logs.app (fun m -> m "got PONG!");
           Luv.Time.sleep 1000;
           Arbiter.send sender_pid (module Ping_msg) (Ping self_pid));
      (Matcher.case (module System.Msg_exit) @@ function
       | `Normal _ -> Logs.app (fun m -> m "normally ended ping")
       | `Error (str, _) ->
           Logs.app (fun m -> m "abruptly ended with message: %s" str);
           Actor.unset_flag ctx `Trap_exit;

           Arbiter.exit self_pid (`Normal self_pid));
    ]

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let _ = Arbiter.spawn (pong ()) in
  Arbiter.run ()
