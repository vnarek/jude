module type ARBITER = sig
  val init : unit -> unit
  val run : unit -> unit
  val spawn : 'a Actor.def -> Pid.t
  val send : Pid.t -> 'a Binable.m -> 'a -> unit
end

module Make_log : functor (B: Backend.B)(Log: Logs.LOG) -> ARBITER
module Make : functor (B : Backend.B) -> ARBITER