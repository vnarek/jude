type messages = (string * bytes) list

type t = messages -> (string * bytes) option

let case (type a) =
  let rec case (m : a Binable.m) fn messages =
    match messages with
    | [] -> None
    | (digest, msg) :: rest -> (
        match Binable.from_bytes m ~digest msg with
        | Error str ->
            Log.debug (fun m -> m "case error: %s" str);
            case m fn rest
        | Ok a ->
            fn a;
            Some (digest, msg) )
  in
  case

let rec react (matchers : t list) messages =
  match matchers with
  | [] -> None
  | m :: rest -> (
      match m messages with
      | None -> react rest messages
      | Some messages -> Some messages )

let sink = function [] -> None | x :: _ -> Some x

let block _ = None
