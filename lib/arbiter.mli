module type ARBITER = sig
  val init : unit -> unit
  val run : unit -> unit
  val spawn : 'a Actor.def -> Pid.t
  val send : Pid.t -> 'a Core_kernel.Binable.m -> 'a -> unit
end

module Make : functor (B : Backend.B) -> ARBITER