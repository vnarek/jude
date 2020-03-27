module type Actor = sig
  type t

  val receive: t -> unit
end

type 'a actor = (module Actor with type t = 'a)

