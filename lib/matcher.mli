type message = string * bytes

type match_result = Matched | Next

type t = message -> match_result

val case : 'a Binable.m -> ('a -> unit) -> t

(** Returns matcher, that calls all matchers defined in [t list]*)
val react : t list -> t

(** Matcher that consumes all messages*)
val sink : t

(** Matcher that consumes no messages*)
val block : t

(** Matcher that can react to any message *)
val any : (string * bytes -> unit) -> t 
