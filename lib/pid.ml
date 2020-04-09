open Core_kernel

type t = {
  address: string;
  port: int;
  id: string
} [@@deriving bin_io]

let to_string {address; port; id} = 
  Printf.sprintf "%s:%d/%s" address port id

let adress_to_string {address; port; _} = Printf.sprintf "%s:%d" address port

let id {id;_} = id

let create ip port id = {
  address = ip;
  port = port;
  id = id
}