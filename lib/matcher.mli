type messages = (string * bytes) list

type t = messages -> messages option

val case : 'a Binable.m -> ('a -> unit) -> t

(** Returns matcher, that calls all matchers defined in [t list]*)
val react : t list -> t

(** Matcher that consumes all messages*)
val sink : t

(** Matcher that consumes no messages*)
val block : t
