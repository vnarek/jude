open Core_kernel

type pid = string [@@deriving bin_io]

type t = {
  pid: pid;
  client: Luv.TCP.t
}


let create pid =
  let client = Backend.create_client () in
  Backend.connect client ignore;
  {
    pid = pid;
    client = client
  }

let get_pid {pid = pid; client = _} = pid

let send (type a) t (m: a Core_kernel.Binable.m) msg =
  let msg_s = Binable.to_string m msg in
  let name = get_pid t in
  let buf = System.msg_to_buffer (System.Msg.ToActor (name, msg_s)) in
  Luv.Stream.write t.client [buf] (fun _ _ -> ())
