module Rwlock = Luv.Rwlock

module type S = sig
  type elt

  type t

  val create : unit -> (t, Luv.Error.t) result

  val push : t -> elt -> unit

  val process_message : t -> (elt list -> elt list) -> unit

  val destroy : t -> unit
end

module Make (O : Set.OrderedType) = struct
  type elt = O.t

  type t = { mutex : Rwlock.t; queue : elt list ref }

  let create () =
    Result.Syntax.(
      let+ mutex = Rwlock.init () in
      { mutex; queue = ref [] })

  let push { mutex; queue } elt =
    Rwlock.wrlock mutex;
    queue := elt :: !queue;
    Rwlock.wrunlock mutex

  let process_message { mutex; queue } fn =
    Rwlock.wrlock mutex;
    let elts = !queue in
    queue := [];
    Rwlock.wrunlock mutex;
    let new_elts = fn elts in
    Rwlock.wrlock mutex;
    queue := List.append !queue new_elts;
    Rwlock.wrunlock mutex

  let destroy t = Rwlock.destroy t.mutex
end
