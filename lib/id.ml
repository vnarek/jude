type t = string [@deriving bin_io]

let gen () = Uuidm.v `V4 |> Uuidm.to_bytes

let to_string t = 
  Uuidm.of_bytes t |> 
  Option.get |> 
  Uuidm.to_string

let of_string t =
  Uuidm.of_string t
  |> Option.get
