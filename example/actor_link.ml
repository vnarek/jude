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


let rec ping ~onfailure num ctx =

  let selfPid = Actor.selfPid ctx in
  if num == 1 then
    onfailure selfPid;
  Matcher.react [
    Matcher.case (module PingMsg) @@ function
    | Ping senderPid -> 
      Logs.app (fun m -> m "got PING!");
      Luv.Time.sleep 1000;
      Arbiter.send senderPid (module PongMsg) (Pong selfPid);
      Actor.become ctx (ping ~onfailure (num - 1))
  ]


let pong () ctx =
  Actor.set_flag ctx `Trap_exit;
  let selfPid = Actor.selfPid ctx in
  let pid = Arbiter.spawn_link selfPid (ping ~onfailure:(fun pid ->  Arbiter.exit pid (`Normal selfPid)) 2) in
  let pid' = Arbiter.spawn_link selfPid (ping ~onfailure:(fun _->  1/0 |> ignore) 2) in
  Arbiter.send pid (module PingMsg) (PingMsg.Ping selfPid);
  Arbiter.send pid' (module PingMsg) (PingMsg.Ping selfPid);
  Matcher.react [
    (Matcher.case (module PongMsg) @@ function
      | Pong senderPid ->
        Logs.app (fun m -> m "got PONG!");
        Luv.Time.sleep 1000;
        Arbiter.send senderPid (module PingMsg) (Ping selfPid));
    Matcher.case (module System.Msg_exit) @@ function
    | `Normal _ -> Logs.app (fun m -> m "normally ended ping")
    | `Error (str, _) ->  
      Logs.app (fun m -> m "abruptly ended with message: %s" str);
      Actor.unset_flag ctx `Trap_exit;

      Arbiter.exit selfPid (`Normal selfPid)
  ]


let () =
  Logs.Src.set_level Jude.Log.log_src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  Arbiter.init();
  let _ = Arbiter.spawn (pong ()) in
  Arbiter.run ()