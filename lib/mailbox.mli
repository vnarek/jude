type 'a t

val create : unit -> ('a t, Luv.Error.t) result

val push : 'a t -> 'a -> unit

val process_message : 'a t -> ('a list -> 'a list) -> unit

val destroy : 'a t -> unit