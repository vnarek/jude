(** Beh module have higher level interfaces over some primitives from Arbiter. *)

(** Module used for supervising actor. It uses monitor primitives.*)
module Supervisor : functor (_ : Arbiter.ARBITER) -> sig
  type recipe = { name : string; public : bool; beh : Actor.beh }
  (** Recipe which is used for creating *)

  type policy =
    | One_for_one
    | All_for_one
        (** [One_for_one] is used when one to one actor should be replaced. When
            [All_for_one] set all actors supervised will be restarted. *)

  type t

  val create : ?policy:policy -> recipe list -> t

  val run : t -> Actor.beh
  (** Run is a behaviour function which should be used for spawning supervisors. *)
end

(** Resolver is used for doing actor pid lookups based on name.*)
module Resolver : functor (_ : Arbiter.ARBITER) -> sig
  val resolve : string -> (Pid.t -> Actor.beh) -> unit
  (** Takes name and function which takes [Pid.t] and return [Actor.beh], which
      is spawned when name is resolvable.*)
end
