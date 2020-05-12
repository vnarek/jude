type t = string -> bytes -> (unit, string) result

val case: 'a Binable.m -> ('a -> unit) -> t
val react: t list -> t
val sink: t
val block: t