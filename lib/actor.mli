type ctx = { 
  selfPid : Pid.t
}

module type Def = sig
  type t [@@deriving bin_io]
  val receive : ctx -> t -> unit
end

type 'a def = (module Def with type t = 'a)

module type Instance = sig
  val receive : Luv.Buffer.t -> unit
  val step : unit -> unit
end

val receive : 'a def -> 'a Mailbox.t -> Luv.Buffer.t -> unit
val create : Pid.t -> 'a def -> (module Instance)
