open Bin_prot.Std

type t = { address : string; port : int; id : string } [@@deriving bin_io]

let to_string { address; port; id } =
  Uuidm.of_bytes id |> Stdlib.Option.get |> Uuidm.to_string
  |> Printf.sprintf "%s:%d/%s" address port

let adress_to_string { address; port; _ } =
  Printf.sprintf "%s:%d" address port

let id { id; _ } = id

let address_port { address; port; _ } = (address, port)

let gen () = Uuidm.v `V4 |> Uuidm.to_bytes

let create ?(id = gen ()) address port = { address; port; id }
