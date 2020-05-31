open Jude

module Backend = Backend.Make ((val Backend.create_config ()))

module Arbiter = Jude.Arbiter.Make (Backend)

module Msg = struct
  type t = First | Second | Third [@@deriving bin_io]
end

let self_message () ctx =
  let self_pid = Actor.self_pid ctx in
  let self_send = Arbiter.send self_pid (module Msg) in
  self_send First;
  Matcher.(
    react
      [
        (case (module Msg) @@ function
         | Msg.First ->
             print_endline "first passed";
             self_send Msg.Second
         | Msg.Second ->
             print_endline "second passed too";
             self_send Msg.Third
         | Msg.Third -> print_endline "all okay!");
      ])

let () =
  Logs.Src.set_level Jude.Log.src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let _ = Arbiter.spawn (self_message ()) in
  Arbiter.run ()
