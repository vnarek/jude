(*let test_hello () = Alcotest.(check string) "should be equal" "Hello Narek"
  (Jude.Hello.hello "Narek") *)

open Jude

module Msg = struct
  type t = First | Second | Third [@@deriving show, eq, ord]
end

module Mailbox = Mailbox.Make (Msg)
