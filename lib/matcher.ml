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
    | Error r ->
      Log.debug (fun m -> m "mismatch: %s" r);
      react rest digest msg
    | Ok() -> Ok()

let sink _ _ = Ok()

let block _ _ = 
  Error "block"
