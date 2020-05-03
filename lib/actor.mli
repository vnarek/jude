type ctx = { 
  selfPid : Pid.t
}

module type DEF = sig
  type t [@@deriving bin_io]
  val receive : ctx -> t -> unit
end

type 'a def = (module DEF with type t = 'a)

type error = Digest_mismatch of string * string

module type INSTANCE = sig
  val receive : digest:string -> bytes -> (unit, error) result
  val step : unit -> unit
end

val receive : 'a def -> 'a Mailbox.t -> bytes -> unit
val create : Pid.t -> 'a def -> (module INSTANCE)
