type string_set = (string, unit) Hashtbl.t

type t = {
  names: (string, Pid.t) Hashtbl.t;
  names_rev: (Pid.t, string_set) Hashtbl.t
}

let create () = {
  names = Hashtbl.create 100;
  names_rev = Hashtbl.create 100
}

let register t name pid =
  Hashtbl.replace t.names name pid; (*TODO: Check and report if already registered *)
  let actual_names = 
    Hashtbl.find_opt t.names_rev pid in
  let ht = match actual_names with
    | Some h -> h
    | None -> Hashtbl.create 100 in
  Hashtbl.add ht name ()

let unregister t name =
  Hashtbl.find_opt t.names name
  |> Option.iter 
    (fun s -> 
       let ht = Hashtbl.find t.names_rev s in
       Hashtbl.remove ht name;
       Hashtbl.remove t.names name;
    )

let unregister_all t pid =
  Hashtbl.find_opt t.names_rev pid
  |> Option.iter (
    Hashtbl.iter (fun a _ -> unregister t a)
  )

let get_name t name =
  Hashtbl.find_opt t.names name