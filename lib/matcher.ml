type message = string * bytes

type match_result = Matched | Next

type t = message -> match_result

let case (type a) (m : a Binable.m) fn (digest, msg) =
  match Binable.from_bytes m ~digest msg with
  | Error str ->
      Log.debug (fun m -> m "case error: %s" str);
      Next
  | Ok a ->
      fn a;
      Matched

let rec react (matchers : t list) messages =
  match matchers with
  | [] -> Next
  | m :: rest -> (
      match m messages with Next -> react rest messages | Matched -> Matched )

let any fn message =
  fn message;
  Matched

let sink = any ignore

let block _ = Next
