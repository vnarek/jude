type actor = Actor.t -> Matcher.t

module type ARBITER = sig
  val init : unit -> unit
  val run : unit -> unit
  val spawn : actor -> Pid.t
  val spawn_link: Pid.t -> actor -> Pid.t
  val send : Pid.t -> 'a Binable.m -> 'a -> unit
  val link: Pid.t -> Pid.t -> unit
  val register: string -> Pid.t -> unit
  val get_name: string -> Pid.t option
end

module Make_log : functor (B: Backend.B)(Log: Logs.LOG) -> ARBITER
module Make : functor (B : Backend.B) -> ARBITER