
(*let test_hello () = Alcotest.(check string) "should be equal" "Hello Narek" (Jude.Hello.hello "Narek") 
*)

open Jude

module Msg = struct
  type t = First | Second | Third [@@deriving show, eq]
end


let test_mailbox () = 
  let mb = Mailbox.create () |> Result.get_ok in
  let xs = [Msg.First; Msg.Second; Msg.Third] in
  List.iter (fun x -> Mailbox.push mb x) xs;
  List.iter (fun x -> 
      let res = Mailbox.take mb |> Option.get in
      Alcotest.(check (module Msg)) "should equal" x res
    ) xs


;;
let () = 
  Alcotest.run "Utils"
    [
      ( "mailbox", 
        [
          Alcotest.test_case "All in all out" `Quick test_mailbox
        ]
      );
    ]