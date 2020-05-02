module Msg = struct
  open Bin_prot.Std
  type t = Adioso of string | Erroroso of int [@@deriving bin_io, show, eq]
end

module Msg2 = struct
  open Bin_prot.Std
  type t = {name: string} [@@deriving bin_io, show]
end

let test_msg_identity_property () =
  Msg.[Adioso "mama mia"; Erroroso 5]
  |> List.iter (fun msg ->
      Jude.Binable.(
        let (_, bytes) = to_bytes (module Msg) msg in
        let msg' = from_bytes (module Msg) bytes |> Result.get_ok in
        Alcotest.(check (module Msg)) "should equal" msg msg'
      )
    )

let test_digest () =
  let (digest, bytes) = Jude.Binable.to_bytes (module Msg) (Msg.Adioso "ahoj") in
  Jude.Binable.from_bytes (module Msg2) ~digest bytes
  |> Result.iter (fun _ -> Alcotest.fail "error")

let tests = [
  "binable",[
    "should msg == to_bytes(from_bytes msg)", `Quick, test_msg_identity_property;
    "should check digest", `Quick, test_digest
  ]
]