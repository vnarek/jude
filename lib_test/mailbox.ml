(*let test_hello () = Alcotest.(check string) "should be equal" "Hello Narek"
  (Jude.Hello.hello "Narek") *)

open Jude

module Msg = struct
  type t = First | Second | Third [@@deriving show, eq]
end

let test_all_in_all_out () =
  let mb = Mailbox.create () |> Result.get_ok in
  let xs = [ Msg.First; Msg.Second; Msg.Third ] in
  List.iter (fun x -> Mailbox.push mb x) xs;
  List.iter
    (fun x ->
      let res = Mailbox.take mb |> Option.get in
      Alcotest.(check (module Msg)) "should equal" x res)
    xs

let tests =
  [ ("mailbox", [ ("all in all out", `Quick, test_all_in_all_out) ]) ]
