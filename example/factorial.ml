open Jude

module Backend = Jude.Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7000
  end)

module Arbiter = Jude.Arbiter.Make(Backend)

module Msg_req = struct
  open Bin_prot.Std

  type t = Do of (Pid.t * int) [@@deriving bin_io]
end

module Msg_res = struct
  open Bin_prot.Std
  type t = Done of (int * int) [@@deriving bin_io]
end

let customer fact_pid num ctx = 
  let selfPid = Actor.selfPid ctx in
  Arbiter.send fact_pid (module Msg_req) (Do (selfPid, num));
  Matcher.(
    react [
      case (module Msg_res) @@ function
      | Done (_, res) -> Logs.app (fun m -> m "ahoj: %d\n" res)
    ]
  )

let rec compute_fact customer num ctx =
  let selfPid = Actor.selfPid ctx in
  if num == 0 then begin
    Arbiter.send customer (module Msg_res) (Done (0, 1));
    Matcher.sink
  end else 
    let _ = Arbiter.spawn (compute_fact selfPid (num - 1)) in
    Matcher.(
      react [
        case (module Msg_res) @@ function
        | Done (n, i) -> Arbiter.send customer (module Msg_res) (Done (n, (num * i)))
      ]
    )



let factorial () _ =
  Matcher.(
    react [
      case (module Msg_req) @@ function
      | Do (customer, num) -> 
        let _ = Arbiter.spawn (compute_fact customer num) in ()
    ]
  )



let () =
  Logs.Src.set_level Jude.Log.log_src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  Arbiter.init();
  let pid = Arbiter.spawn (factorial ()) in
  let _ = Arbiter.spawn (customer pid 5) in
  Arbiter.run ()