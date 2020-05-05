type ctx = { 
  selfPid : Pid.t
}

type t

type error = Digest_mismatch of string * string

val create : unit -> t
val receive: t -> string -> bytes -> unit
val init : (ctx -> Matcher.t) -> t -> Pid.t -> unit
val step: t -> (unit, string) result
