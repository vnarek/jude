module type ARBITER = sig
  val run : unit -> unit

  val spawn : Actor.beh -> Pid.t

  val spawn_link : Pid.t -> Actor.beh -> Pid.t

  val send : Pid.t -> 'a Binable.m -> 'a -> unit

  val link : Pid.t -> Pid.t -> unit

  val register : public:bool -> string -> Pid.t -> unit

  val unregister : string -> unit

  val get_name : string -> Pid.t option

  val resolve_name : string -> Pid.t -> unit

  val exit : Pid.t -> System.Msg_exit.t -> unit

  val unmonitor : Pid.t -> Pid.t -> unit

  val monitor : Pid.t -> Pid.t -> unit

  val become : Actor.t -> Actor.beh -> unit
end

module Make : functor (_ : Backend.B) -> ARBITER
