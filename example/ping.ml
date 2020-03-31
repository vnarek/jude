
open Jude

module Ping = struct
  type t = Ping [@@deriving bin_io]

  let receive = function
    | Ping -> print_endline "got DO!"
end


module MakePong(P: Arbiter.PID with type t = Ping.t) = struct 
  type t = Pong [@@deriving bin_io]

  let receive = function
    | Pong -> 
      print_endline "got pong!";
      Luv.Time.sleep 5000;
      P.send Ping.Ping
end


let create_pong (module P: Arbiter.PID with type t = Ping.t) =
  let module T = struct
    type t = Pong [@@deriving bin_io]

    let receive = function
      | Pong -> 
        print_endline "got pong!";
        Luv.Time.sleep 5000;
        P.send Ping.Ping
  end
  in 
  (module T: Arbiter.Def)

let () = 
  Arbiter.init();

  let (module P) = Arbiter.spawn (module Ping) "ping" in
  let module Pong = MakePong(P) in
  let (module P') = Arbiter.spawn (module Pong) "pong" in

  ignore(Luv.Thread.create (
      fun() ->
        Luv.Time.sleep 2000;
        print_endline "shiet";
        P'.send Pong.Pong
    ), ignore
    );
  Arbiter.run ()