include Stdlib.Result

exception Unwrap_error of string

let unwrap msg r =
  match r with Ok v -> v | Error _ -> raise @@ Unwrap_error msg

module Syntax = struct
  let ( let+ ) r f = map f r

  let ( >>= ) = bind
end
