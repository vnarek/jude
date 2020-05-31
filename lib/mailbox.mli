type 'a t

val create : unit -> ('a t, Luv.Error.t) result

val push : 'a t -> 'a -> unit

val take : 'a t -> 'a option

val filter : 'a t -> ('a -> bool) -> unit

val destroy : 'a t -> unit

val process_message: 'a t -> ('a list -> 'a list option) -> unit
