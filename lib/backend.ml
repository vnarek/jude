type t = Luv.TCP.t

let server_ip = "127.0.0.1"
let server_port = 7000

let server_address = Luv.Sockaddr.ipv4 server_ip server_port |> Result.get_ok

let get_address_s address =
  let address_s = Luv.Sockaddr.to_string address |> Option.get in
  let port = Luv.Sockaddr.port address
             |> Option.map (fun o -> Int.to_string o) in
  match port with
  | Some p -> String.concat ":" [address_s; p]
  | None -> address_s



let create_arbiter ?loop () = 
  let server = Luv.TCP.init ?loop () |> Result.get_ok in
  let _ = Luv.TCP.bind server server_address in
  server

let listen ?loop server fn = 
  Luv.Stream.listen server begin function
    | Error e ->
      Printf.eprintf "Listen error: %s\n" (Luv.Error.strerror e)
    | Ok () ->
      let client = Luv.TCP.init ?loop () |> Result.get_ok in

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
let create_client ?loop () =
  Luv.TCP.init () ?loop:loop |> Result.get_ok


let connect client fn = Luv.TCP.connect client server_address begin function
    | Error e ->
      Printf.eprintf "Connect error: %s\n" (Luv.Error.strerror e)
    | Ok () -> fn client
  end

