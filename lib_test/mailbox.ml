open Jude

let test_message_order () =
  let m = Mailbox.create () |> Result.get_ok in
  Mailbox.push m "message";
  Mailbox.push m "message2";
  Mailbox.process_message m (function
    | [] -> Alcotest.fail "expected 2 messages"
    | _ :: rest ->
        Mailbox.push m "message3";
        rest);
  Mailbox.process_message m (function
    | [] -> Alcotest.fail "expected 2 messages"
    | msg :: rest ->
        Alcotest.(check string "should by equal" "message3" msg);
        rest)

let tests =
  [
    ("mailbox", [ ("message order should match", `Quick, test_message_order) ]);
  ]
