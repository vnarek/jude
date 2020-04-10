module type CONFIG = sig
  val server_ip: string
  val server_port: int
end

module DefaultConfig : CONFIG = struct
  let server_ip = "127.0.0.1"
  let server_port = 7000
end

module type B = sig
  type t = Luv.TCP.t

  val server_ip : string
  val server_port : int
  val server_address : Luv.Sockaddr.t
  val server_address_s : string
  val create_arbiter : unit -> Luv.TCP.t
  val listen : [ `TCP ] Luv.Stream.t -> (Luv.TCP.t -> Luv__.Buffer.t -> unit) -> unit
  val connect : (string * int) -> (Luv.TCP.t -> unit) -> Luv.TCP.t
end

module Make(C: CONFIG): B = struct

  include C

  type t = Luv.TCP.t

  let server_address = Luv.Sockaddr.ipv4 C.server_ip C.server_port |> Result.get_ok

  let server_address_s = String.concat ":" [C.server_ip; Int.to_string C.server_port]

  let create_arbiter () = 
    let server = Luv.TCP.init () |> Result.get_ok in
    let _ = Luv.TCP.bind server server_address in
    server

  let listen server fn = 
    Luv.Stream.listen server begin function
      | Error e ->
        Printf.eprintf "Listen error: %s\n" (Luv.Error.strerror e)
      | Ok () ->
        let client = Luv.TCP.init () |> Result.get_ok in

        match Luv.Stream.accept ~server ~client with
        | Error e ->
          Luv.Error.strerror e |> print_endline;
          Luv.Handle.close client ignore
        | Ok () ->
          Luv.Stream.read_start client begin function
            | Error `EOF ->
              Luv.Handle.close client ignore
            | Error e ->
              Printf.eprintf "Read error: %s\n" (Luv.Error.strerror e);
              Luv.Handle.close client ignore
            | Ok buffer ->
              fn client buffer
          end;
    end

  let connect (address, port) fn =
    let client = Luv.TCP.init () |> Result.get_ok in
    let sock_addr = Luv.Sockaddr.ipv4 address port |> Result.get_ok in 
    Luv.TCP.connect client sock_addr (fun e -> 
        match e with
        | Error e ->
          Printf.printf "Connect error: %s\n" (Luv.Error.strerror e)
        | Ok () ->
          fn client);
    client
end