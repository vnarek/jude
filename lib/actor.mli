type ctx = { 
  selfPid : Pid.t
}

type t = {
  mailbox: (string * bytes) Mailbox.t;
  mutable cont: Matcher.t ref;
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
val create : 'a def -> (t * (module INSTANCE))
val init : 'a def -> t -> Pid.t -> unit
