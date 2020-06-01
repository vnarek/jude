module Rwlock = Luv.Rwlock

module type S = sig
  type elt

  type t

  val create : unit -> (t, Luv.Error.t) result

  val push : t -> elt -> unit

  val process_message : t -> (elt list -> elt option) -> unit

  val destroy : t -> unit
end

module Make (O : Set.OrderedType) = struct
  module Multiset = Multiset.Make (O)

  type elt = O.t

  type t = { mutex : Rwlock.t; queue : Multiset.t ref }

  let create () =
    Result.Syntax.(
      let+ mutex = Rwlock.init () in
      { mutex; queue = ref Multiset.empty })

  let push { mutex; queue } elt =
    Rwlock.wrlock mutex;
    queue := Multiset.add !queue elt;
    Rwlock.wrunlock mutex

  let process_message { mutex; queue } fn =
    Rwlock.wrlock mutex;
    let elts = Multiset.to_list !queue in
    Rwlock.wrunlock mutex;
    let o = fn elts in
    Rwlock.wrlock mutex;
    Option.iter (fun el -> queue := Multiset.remove !queue el) o;
    Rwlock.wrunlock mutex

  let destroy t = Rwlock.destroy t.mutex
end
