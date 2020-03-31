module type Def = sig
  type t [@@deriving bin_io]

  val receive: t -> unit
end

type 'a def = (module Def with type t = 'a)

