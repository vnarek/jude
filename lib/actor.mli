type t

type error = Digest_mismatch of string * string

val create : Pid.t -> t
val receive: t -> string -> bytes -> unit
val init : (t -> Matcher.t) -> t -> unit
val step: t -> (unit, string) result
val selfPid: t -> Pid.t
val become: t -> (t -> Matcher.t) -> unit
