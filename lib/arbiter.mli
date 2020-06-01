module type ARBITER = sig
  val run : unit -> unit
  (** Run starts the event loop and block *)

  val spawn : Actor.beh -> Pid.t
  (** Takes [Actor.beh] and spawns it. It returns pid which can be used for
      sending messages to the actor*)

  val spawn_link : Pid.t -> Actor.beh -> Pid.t

  val send : Pid.t -> 'a Binable.m -> 'a -> unit
  (** Send is used to send messages to a [Binable.m] messages. *)

  val link : Pid.t -> Pid.t -> unit
  (** Link two pids together. When actor linked to that pid exits, it sends a
      signal which kills the second one too. This behaviour can be supressed
      when using [`Trap_exit] flag. Links are bidirectional.*)

  val register : public:bool -> string -> Pid.t -> unit
  (** Add name to a pid for easier retrieve. When name is registered arbiter
    sends this information to other arbiters. *)

  val unregister : string -> unit
  (** Remove name and sends message indicating that to arbiters. *)
  val get_name : string -> Pid.t option

  val resolve_name : string -> Pid.t -> unit
  (** Subscribe to a name. When name is subscribed message is send to [Pid.t]*)
  val exit : Pid.t -> System.Exit_msg.t -> unit
  (** Exit autor identified by [Pid.t] with reason in [System.Msg_exit.t] *)
  val monitor : Pid.t -> Pid.t -> unit
  (** Monitor sets unidirectional connection from first pid to the second.
    It does not automatically end itself when message came.*)
  val unmonitor : Pid.t -> Pid.t -> unit

  val become : Actor.t -> Actor.beh -> unit
  (** Change behaviour of an actor. Used to change what messages actor takes. *)
end

module Make : functor (_ : Backend.B) -> ARBITER
