let (let+) r f = Result.map f r


type 'a t = {
  mutex: Luv.Rwlock.t;
  queue: 'a Queue.t
}

let create () = 
  let+ mutex = Luv.Rwlock.init () in
  let queue = Queue.create () in
  {
    mutex;
    queue
  }

let push {mutex; queue} msg =
  Luv.Rwlock.wrlock mutex;
  Queue.push msg queue;
  Luv.Rwlock.wrunlock mutex

let take {mutex; queue} =
  Luv.Rwlock.rdlock mutex;
  let msg = Queue.take_opt queue in
  Luv.Rwlock.rdunlock mutex;
  msg

(*TODO: rewrite Mailbox to add filter and stuff *)
let destroy t = Luv.Rwlock.destroy t.mutex 