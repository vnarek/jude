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
  val listen : (Luv.TCP.t -> Luv.Buffer.t -> unit) -> unit
  val connect : (string * int) -> Luv.TCP.t
  val send: (string * int) -> Luv.Buffer.t -> unit
end


module Make_log(C: CONFIG)(Log: Logs.LOG): B = struct

  include C

  type t = Luv.TCP.t

  type state = {
    server: Luv.TCP.t;
    clients: ((string * int), Luv.TCP.t) Hashtbl.t;
    send_ch: ((string * int) * Luv.Buffer.t) Channel.t
  }

  let server_address = Luv.Sockaddr.ipv4 C.server_ip C.server_port |> Result.get_ok

  let create_arbiter () = 
    let server = Luv.TCP.init () |> Result.get_ok in
    let _ = Luv.TCP.bind server server_address in
    server

  let state = {
    server = create_arbiter ();
    clients = Hashtbl.create 100;
    send_ch = Channel.create ()
  }

  let report_err str e = Logs.err @@ fun m -> m str (Luv.Error.strerror e)

  let connect (address, port) =
    let client = Luv.TCP.init () |> Result.get_ok in
    let sock_addr = Luv.Sockaddr.ipv4 address port |> Result.get_ok in 
    Luv.TCP.connect client sock_addr (fun e -> 
        match e with
        | Error e ->
          report_err "Connect error: %s\n" e
        | Ok () -> Luv.Stream.read_start client begin function
            | Error e ->
              report_err "Read error: %s\n" e;
              Hashtbl.remove state.clients (address, port);
              Luv.Handle.close client ignore
            | _ -> ()
          end;
      );
    client

  let async_send_to_client =
    Luv.Async.init (fun _ ->
        Channel.consume state.send_ch (fun (destination, buf) ->
            let client = match Hashtbl.find_opt state.clients destination with
              | Some client -> client
              | None ->
                let client = connect destination in
                Hashtbl.add state.clients destination client;
                client
            in
            Luv.Stream.write client [buf] (fun e _ -> 
                match e with 
                | Error e -> report_err "Write error: %s\n" e
                | Ok () -> ()
              );
          );
      ) |> Result.get_ok

  let send destination buf = 
    Channel.send state.send_ch (destination, buf);
    Luv.Async.send async_send_to_client 
    |> Result.iter_error (report_err "Send error: %s \n")

  let listen fn = 
    Luv.Stream.listen state.server begin function
      | Error e ->
        report_err "Listen error: %s\n" e
      | Ok () ->
        let client = Luv.TCP.init () |> Result.get_ok in

        match Luv.Stream.accept ~server:state.server ~client with
        | Error e ->
          report_err "Accept error: %s\n" e;
          Luv.Handle.close client ignore
        | Ok () ->
          Luv.Stream.read_start client begin function
            | Error `EOF ->
              Log.debug (fun m -> m "client hangup");
              Luv.Handle.close client ignore
            | Error e ->
              report_err "Read error: %s\n" e;
              Luv.Handle.close client ignore
            | Ok buffer ->
              fn client buffer
          end;
    end
end

module Make(C: CONFIG) = Make_log(C)(Log.Log)
