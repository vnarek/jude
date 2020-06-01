type messages = (string * bytes) list

type matched = { matched : string * bytes; rest : messages }

type match_result = Matched of matched | Next

type t = messages -> match_result

let case (type a) =
  let case (m : a Binable.m) fn messages =
    let rec case acc (m : a Binable.m) fn messages =
      match messages with
      | [] -> Next
      | (digest, msg) :: rest -> (
          match Binable.from_bytes m ~digest msg with
          | Error str ->
              Log.debug (fun m -> m "case error: %s" str);
              case ((digest, msg) :: acc) m fn rest
          | Ok a ->
              fn a;
              Matched { matched = (digest, msg); rest = List.append acc rest } )
    in
    case [] m fn messages
  in
  case

let rec react (matchers : t list) messages =
  match matchers with
  | [] -> Next
  | m :: rest -> (
      match m messages with
      | Next -> react rest messages
      | Matched m -> Matched m )

let sink = function [] -> Next | matched :: rest -> Matched { matched; rest }

let block _ = Next
