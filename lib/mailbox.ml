let (let+) r f = Result.map f r


type 'a t = {
  mutex: Luv.Rwlock.t;
  queue: 'a list ref
}

let create () = 
  let+ mutex = Luv.Rwlock.init () in
  {
    mutex;
    queue=ref []
  }

let push {mutex; queue} msg =
  Luv.Rwlock.wrlock mutex;
  queue := msg :: !queue;
  Luv.Rwlock.wrunlock mutex

let take {mutex; queue} =
  Luv.Rwlock.wrlock mutex;
  let msg = match !queue with
    | x :: xs ->
      queue := xs;
      Some(x)
    | _ -> None
  in
  Luv.Rwlock.wrunlock mutex;
  msg

let filter {mutex; queue} fn =
  Luv.Rwlock.wrlock mutex;
  let new_list = List.filter fn !queue in
  queue := new_list;
  Luv.Rwlock.wrunlock mutex

let destroy t = Luv.Rwlock.destroy t.mutex 