type pid_id = String.t

type actor = {
  id: pid_id;
  step: unit -> unit;
}

type 't pid = {
  send: 't -> unit;
}

(* System.t *)
type t = {
  queue: actor Queue.t
}

let state = {
  queue = Queue.create ()
}

let rec run ()= 
  match Queue.take_opt state.queue with
  | Some ac -> 
    ac.step ();
    run ()
  | None -> ()

let spawn f =
  let m_box = Queue.create () in
  let actor = {
    id = "adioso";
    step = (fun () -> f (Queue.pop m_box))
  } 
  in
  {
    send = (fun msg -> 
        Queue.push msg m_box;
        Queue.push actor state.queue
      );
  }

let howdy_fn = function
    `Howdy -> Printf.printf "hello bois"

let howdy_create = (fun () ->
    let counter = ref 0 in
    fun msg ->
      match msg with
      | `Incr x -> counter := !counter + x
      | `Print  -> 
        Printf.printf "%d\n" !counter
  )

let () =
  let pid = spawn (howdy_create ()) in
  let pid' = spawn (howdy_create ()) in
  pid.send (`Incr 5);
  pid'.send (`Incr 10);
  pid.send (`Print);
  pid'.send(`Print);
  pid'.send (`Incr 10);
  run ()
