type messages = (string * bytes) list

type matched = { matched : string * bytes; rest : messages }

type match_result = 
    | Matched of matched
    | Next

type t = messages -> match_result

val case : 'a Binable.m -> ('a -> unit) -> t

(** Returns matcher, that calls all matchers defined in [t list]*)
val react : t list -> t

(** Matcher that consumes all messages*)
val sink : t

(** Matcher that consumes no messages*)
val block : t

val any : (string * bytes -> unit) -> t 
