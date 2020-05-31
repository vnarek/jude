type messages = (string * bytes) list

type t = messages -> messages option

let case (type a) =
  let rec case (m : a Binable.m) ?(acc = []) fn messages =
    match messages with
    | [] -> None
    | (digest, msg) :: rest -> (
        match Binable.from_bytes m ~digest msg with
        | Error str ->
            Log.debug (fun m -> m "case error: %s" str);
            case m fn rest ~acc:((digest, msg) :: acc)
        | Ok a ->
            fn a;
            Some rest )
  in
  case ~acc:[]

let rec react (matchers : t list) messages =
  match matchers with
  | [] -> None
  | m :: rest -> (
      match m messages with
      | None -> react rest messages
      | Some messages -> Some messages )

let sink _ = Some []

let block msgs = Some msgs
