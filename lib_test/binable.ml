module Msg = struct
  open Bin_prot.Std
  type t = Adioso of string | Erroroso of int [@@deriving bin_io, show, eq]
end

let test_msg_identity_property () =
  Msg.[Adioso "mama mia"; Erroroso 5]
  |> List.iter (fun msg ->
      Jude.Binable.(
        let bytes = to_bytes (module Msg) msg in
        let msg' = from_bytes (module Msg) bytes in
        Alcotest.(check (module Msg)) "should equal" msg msg'
      )
    )

let tests = [
  "binable",[
    "should msg == to_bytes(from_bytes msg)", `Quick, test_msg_identity_property
  ]
]