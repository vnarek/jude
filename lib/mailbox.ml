module Rwlock = Luv.Rwlock

type 'a t = { mutex : Rwlock.t; queue : 'a list ref }

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
