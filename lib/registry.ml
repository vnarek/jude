type string_set = (string, unit) Hashtbl.t

type record = { pid : Pid.t; is_local : bool }

type t = {
  names : (string, record) Hashtbl.t;
  names_rev : (Pid.t, string_set) Hashtbl.t;
  on_name : (string, (Pid.t -> unit) list) Hashtbl.t;
}

let create () =
  {
    names = Hashtbl.create 100;
    names_rev = Hashtbl.create 100;
    on_name = Hashtbl.create 100;
  }

let get_name t name =
  Hashtbl.find_opt t.names name |> Option.map (fun re -> re.pid)

let get_public_names t =
  Hashtbl.to_seq t.names
  |> Seq.filter (fun (_, re) -> re.is_local)
  |> Seq.map (fun (name, re) -> (name, re.pid))
  |> List.of_seq

let on_name t name fn =
  match get_name t name with
  | Some pid -> fn pid
  | None ->
      Hashtbl.find_opt t.on_name name
      |> Option.map @@ List.cons fn
      |> Option.value ~default:[ fn ]
      |> Hashtbl.replace t.on_name name

let process_on_name_callbacks t name pid =
  ( Hashtbl.find_opt t.on_name name
  |> Option.to_list |> List.flatten
  |> List.iter @@ fun clb ->
     Log.debug (fun m -> m "count");
     clb pid );
  Hashtbl.remove t.on_name name

let register ~local t name pid =
  Hashtbl.replace t.names name
    {
      pid;
      (*TODO: Check and report if already registered *)
      is_local = local;
    };
  let pid_names = Hashtbl.find_opt t.names_rev pid in
  let ht = match pid_names with Some h -> h | None -> Hashtbl.create 100 in
  Hashtbl.add ht name ();
  process_on_name_callbacks t name pid

let unregister t name =
  Hashtbl.find_opt t.names name
  |> Option.iter (fun re ->
         let ht = Hashtbl.find t.names_rev re.pid in
         Hashtbl.remove ht name;
         Hashtbl.remove t.names name)

let unregister_all t pid =
  Hashtbl.find_opt t.names_rev pid
  |> Option.iter (Hashtbl.iter (fun a _ -> unregister t a))
