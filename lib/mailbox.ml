module Rwlock = Luv.Rwlock

type 'a t = { mutex : Rwlock.t; queue : 'a Fqueue.t ref }

let create () =
  Result.Syntax.(
    let+ mutex = Rwlock.init () in
    { mutex; queue = ref Fqueue.empty })

let push { mutex; queue } elt =
  Rwlock.wrlock mutex;
  queue := Fqueue.cons elt !queue;
  Rwlock.wrunlock mutex

type 'a matches = Matched of 'a Fqueue.t | Next

let rec try_match ?(deffered = []) elts fn =
  match Fqueue.take_back elts with
  | None -> Next
  | Some (rest, e) ->
      if fn e then
        Matched (List.fold_left (fun q n -> Fqueue.cons n q) rest deffered)
      else try_match ~deffered:(e :: deffered) rest fn

let rec try_all elts fn =
  match try_match elts fn with Matched elts -> try_all elts fn | Next -> elts

let process_message { mutex; queue } fn =
  Rwlock.wrlock mutex;
  let elts = !queue in
  queue := Fqueue.empty;
  Rwlock.wrunlock mutex;
  let new_elts = try_all elts fn in
  Rwlock.wrlock mutex;
  queue := Fqueue.append new_elts !queue;
  Rwlock.wrunlock mutex

let destroy t = Rwlock.destroy t.mutex
