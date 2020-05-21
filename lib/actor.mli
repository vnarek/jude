(** Module defines data structure representing actor internal [t] and public
    data types [beh]*)

type t

type beh = t -> Matcher.t

type process_flags = [ `Trap_exit ]
(** Flags that are set using [set_flag] *)

val create : Pid.t -> t
(** Create is used to create new [t] *)

val receive : t -> string -> bytes -> unit

val init : (t -> Matcher.t) -> t -> unit

val step : t -> unit

val self_pid : t -> Pid.t
(** Returns pid of an actor *)

val become : t -> (t -> Matcher.t) -> unit

val link : t -> Pid.t -> unit

val link_iter : (Pid.t -> unit) -> t -> unit

val set_flag : t -> process_flags -> unit

val has_flag : t -> process_flags -> bool

val unset_flag : t -> process_flags -> unit

val monitor : Pid.t -> t -> unit

val unmonitor : Pid.t -> t -> unit

val monitor_iter : (Pid.t -> unit) -> t -> unit
