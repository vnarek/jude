open Jude

module Msg = struct
  open Bin_prot.Std

  type t = Adioso of string [@@deriving bin_io, eq, show]
end

module Timer = struct
  type t = Finished [@@deriving bin_io, eq, show]
end

let receive (type a) (m : a Binable.m) t msg =
  let digest, buf = Binable.to_bytes m msg in
  Actor.receive t digest buf

let test_msg_order_of_matching () =
  let test_actor () _ctx =
    Matcher.(
      react
        [
          case (module Timer) (function Finished -> ());
          case
            (module Msg)
            (function Adioso _str -> Alcotest.fail "should take timer first");
        ])
  in
  let pid = Pid.create ("127.0.0.1", 7000) in
  let t = Actor.create pid in
  let beh = test_actor () in
  Actor.init beh t;
  receive (module Timer) t Finished;
  receive (module Msg) t (Adioso "ola!");
  Actor.step t

let tests =
  [
    ( "actor",
      [ ("message order should match", `Quick, test_msg_order_of_matching) ] );
  ]
