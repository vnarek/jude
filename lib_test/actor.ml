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

let create_actor beh =
  let pid = Pid.create ("127.0.0.1", 7000) in
  let t = Actor.create pid in
  Actor.init beh t;
  t

let test_msg_order_of_matching () =
  let timed = ref false in
  let test_actor () _ctx =
    Matcher.(
      react
        [
          case (module Timer) (function Finished -> timed := true);
          case
            (module Msg)
            (function
              | Adioso _str ->
                  if not !timed then Alcotest.fail "should take timer first");
        ])
  in
  let t = create_actor (test_actor ()) in
  receive (module Timer) t Finished;
  receive (module Msg) t (Adioso "ola!");
  Actor.step t

let test_not_matched_msgs_stay () =
  let adiosed = ref false in
  let test_actor () _ctx =
    Matcher.(
      react
        [
          case (module Timer) (function Finished -> ());
          case (module Msg) (function Adioso _str -> adiosed := true);
        ])
  in
  let t = create_actor (test_actor ()) in
  receive (module Timer) t Finished;
  receive (module Msg) t (Adioso "ola!");
  Actor.step t;
  Actor.step t;
  if not !adiosed then Alcotest.fail "should matched adiosed"

let test_same_message_count () =
  let msg_num = ref 0 in
  let test_actor () _ctx =
    Matcher.(
      react
        [ case (module Timer) (function Finished -> msg_num := !msg_num + 1) ])
  in
  let t = create_actor (test_actor ()) in
  receive (module Timer) t Finished;
  receive (module Timer) t Finished;
  receive (module Timer) t Finished;
  Actor.step t;
  Alcotest.(check int "should equal" 3 !msg_num)

let tests =
  [
    ( "actor",
      [
        ("message order should match", `Quick, test_msg_order_of_matching);
        ("message should not disapear", `Quick, test_not_matched_msgs_stay);
        ("equal messages should not be ignored", `Quick, test_same_message_count);
      ] );
  ]
