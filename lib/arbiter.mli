
val init: unit -> unit

val run: unit -> unit

module type Def = sig
  type t [@@deriving bin_io]
  val receive: t -> unit
end

module type PID = sig
  type t
  val name: string
  val send: t -> unit
end

type 'a def = (module Def with type t = 'a)
type 'a pid = (module PID with type t = 'a)

(*val send: 'b Luv.Stream.t -> 'a def -> 'a -> unit*)
val spawn: 'a def -> string -> 'a pid
