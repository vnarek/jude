type ctx = { 
  selfPid : Pid.t
}

type t

module type DEF = sig
  type t [@@deriving bin_io]
  val receive : ctx -> Matcher.t
end

type 'a def = (module DEF with type t = 'a)

type error = Digest_mismatch of string * string

val create : 'a def -> t
val receive: t -> string -> bytes -> unit
val init : 'a def -> t -> Pid.t -> unit
val step: t -> (unit, string) result
