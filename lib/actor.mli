type ctx = { 
  selfPid : Pid.t
}

module type DEF = sig
  type t [@@deriving bin_io]
  val receive : ctx -> Matcher.t
end

type 'a def = (module DEF with type t = 'a)

type error = Digest_mismatch of string * string

module type INSTANCE = sig
  val receive : digest:string -> bytes -> unit
  val step : unit -> (unit, string) result
end

val receive : 'a def -> 'a Mailbox.t -> bytes -> unit
val create : Pid.t -> 'a def -> (module INSTANCE)
