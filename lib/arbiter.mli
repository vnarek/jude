module type ARBITER = sig
  val find_instance : string -> (module Actor.INSTANCE)
  val init : unit -> unit
  val run : unit -> unit
  val spawn : 'a Actor.def -> string -> Pid.t
  val send : Pid.t -> 'a Core_kernel.Binable.m -> 'a -> unit
end

module Make : functor (B : Backend.B) -> ARBITER