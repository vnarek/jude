type t

type error = Digest_mismatch of string * string
type process_flags = [`Trap_exit]

val create : Pid.t -> t
val receive: t -> string -> bytes -> unit
val init : (t -> Matcher.t) -> t -> unit
val step: t -> (unit, string) result
val selfPid: t -> Pid.t
val become: t -> (t -> Matcher.t) -> unit
val link: t -> Pid.t -> unit
val link_iter: (Pid.t -> unit) -> t -> unit
val set_flag: t -> process_flags -> unit
val has_flag: t -> process_flags -> bool
val unset_flag: t -> process_flags -> unit
val monitor: Pid.t -> t -> unit
val unmonitor: Pid.t -> t -> unit
val monitor_iter: (Pid.t -> unit) -> t -> unit