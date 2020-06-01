module Rwlock = Luv.Rwlock

module type S = sig
  type elt

  type t

  val create : unit -> (t, Luv.Error.t) result

  val push : t -> elt -> unit

  val process_message : t -> (elt list -> elt option) -> unit

  val destroy : t -> unit
end

module Make (O : Set.OrderedType) : S with type elt = O.t