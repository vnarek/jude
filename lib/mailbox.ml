module Rwlock = Luv.Rwlock

let ( let+ ) r f = Result.map f r

type 'a t = { mutex : Rwlock.t; queue : 'a list ref }

let create () =
  let+ mutex = Rwlock.init () in
  { mutex; queue = ref [] }

let push { mutex; queue } msg =
  Rwlock.wrlock mutex;
  queue := msg :: !queue;
  Rwlock.wrunlock mutex

let take { mutex; queue } =
  Rwlock.wrlock mutex;
  let msg =
    match !queue with
    | x :: xs ->
        queue := xs;
        Some x
    | _ -> None
  in
  Rwlock.wrunlock mutex;
  msg

let process_message { mutex; queue } fn =
  Rwlock.wrlock mutex;
  let msgs = !queue in
  queue := [];
  Rwlock.wrunlock mutex;
  let rest = Option.value ~default:msgs (fn msgs) in
  Rwlock.wrlock mutex;
  queue := List.append !queue rest;
  Rwlock.wrunlock mutex

let filter { mutex; queue } fn =
  Rwlock.wrlock mutex;
  let new_list = List.filter (fun m -> not (fn m)) !queue in
  queue := new_list;
  Rwlock.wrunlock mutex

let destroy t = Rwlock.destroy t.mutex
