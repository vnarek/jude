
val init: unit -> unit

val run: unit -> unit

(*val send: 'b Luv.Stream.t -> 'a def -> 'a -> unit*)
val spawn: 'a Actor.def -> string -> Pid.t

val send: Pid.t -> 'a Core_kernel.Binable.m -> 'a -> unit