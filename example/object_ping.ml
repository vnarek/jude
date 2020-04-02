
module type PID = sig
  type t

  val receive: t -> unit
end

module Ping = struct
  type t = Ping

  let receive = function
    | Ping -> print_endline "got DO!"
end

let p = (module Ping: PID with type t = Ping.t)

let j = Marshal.to_string p [Marshal.Closures]

let () =
  let (module P) = p in
  P.receive Ping.Ping