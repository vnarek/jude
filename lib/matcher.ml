
module Msg = struct
  open Bin_prot.Std

  type t = Win of string [@@deriving bin_io]
end


type t = string -> bytes -> (unit, string) result

let case (type a) (m: a Binable.m) fn digest msg =
  let res = Binable.from_bytes m ~digest msg in
  Result.iter fn res;
  Result.bind res (fun _ -> Ok())

let rec react matchers digest msg =
  match matchers with
  | [] ->
    Error "no match"
  | m :: rest ->
    match m digest msg with
    | Error _ -> react rest digest msg
    | _ -> Ok()

let sink _ _ = Ok()
