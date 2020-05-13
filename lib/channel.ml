module SyncQueue = struct
  type 'a t = { q : 'a Queue.t; mux : Luv.Mutex.t }

  let create () =
    let mux = Luv.Mutex.init () |> Result.unwrap "mutex init" in
    { q = Queue.create (); mux }

  let send t a =
    Luv.Mutex.lock t.mux;
    Queue.push a t.q;
    Luv.Mutex.unlock t.mux

  let pop t =
    Luv.Mutex.lock t.mux;
    let a = Queue.take_opt t.q in
    Luv.Mutex.unlock t.mux;
    a
end

type 'a t = {
  queue : 'a SyncQueue.t;
  mux : Luv.Mutex.t;
  cond : Luv.Condition.t;
  mutable closed : bool ref;
}

let create () =
  let mux = Luv.Mutex.init () |> Result.unwrap "mutex init" in
  let cond = Luv.Condition.init () |> Result.unwrap "mutex init" in
  { queue = SyncQueue.create (); mux; cond; closed = ref false }

let send t a =
  SyncQueue.send t.queue a;
  Luv.Condition.signal t.cond

let rec recv t =
  Luv.Mutex.lock t.mux;
  match SyncQueue.pop t.queue with
  | Some x ->
      Luv.Mutex.unlock t.mux;
      Some x
  | None ->
      Luv.Condition.wait t.cond t.mux;
      Luv.Mutex.unlock t.mux;
      recv t

let close t =
  Luv.Mutex.lock t.mux;
  t.closed := true;
  Luv.Mutex.unlock t.mux

let rec consume t fn =
  match SyncQueue.pop t.queue with
  | Some x ->
      fn x;
      consume t fn
  | None -> ()
