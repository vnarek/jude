module SyncQueue = struct
  type 'a t = {
    q: 'a Queue.t;
    mux: Luv.Mutex.t;
  }

  let create() = 
    let mux = Luv.Mutex.init() |> Result.get_ok in
    {
      q = Queue.create();
      mux = mux;
    }

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
  queue: 'a SyncQueue.t;
  mux: Luv.Mutex.t;
  cond: Luv.Condition.t;
  mutable closed: bool ref
}

(*
val create: unit -> 'a t
val send: 'a -> unit
val recv: unit -> 'a option
val close: unit -> unit
*)

let create () = 
  let mux = Luv.Mutex.init () |> Result.get_ok in
  let cond = Luv.Condition.init () |> Result.get_ok in
  {
    queue = SyncQueue.create ();
    mux = mux;
    cond = cond;
    closed = ref false
  }

let send t a = 
  SyncQueue.send t.queue a;
  Luv.Condition.signal t.cond

(*let rec wait_until cond mux f =*)


let rec recv t =
  Luv.Mutex.lock t.mux;
  match SyncQueue.pop t.queue with
  | Some x ->
    Luv.Mutex.unlock t.mux; 
    Some(x)
  | None ->
    Luv.Condition.wait t.cond t.mux; (* Research how conds work *)
    Luv.Mutex.unlock t.mux;
    recv t

let close t = 
  Luv.Mutex.lock t.mux;
  t.closed := true;
  Luv.Mutex.unlock t.mux