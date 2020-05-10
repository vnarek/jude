open Log

module type CONFIG = sig
  val server_ip: string
  val server_port: int
end

module DefaultConfig : CONFIG = struct
  let server_ip = "127.0.0.1"
  let server_port = 7000
end

module type B = sig
  val server_ip : string
  val server_port : int
  val server_address : Luv.Sockaddr.t
  val start: (Luv.TCP.t -> Luv.Buffer.t -> unit) -> unit
  val connect : (string * int) -> Luv.TCP.t
  val send: (string * int) -> Luv.Buffer.t -> unit
end

let report_err str e = Logs.err @@ fun m -> m str (Luv.Error.strerror e)

module Discovery_msg = struct
  open Bin_prot.Std

  type t = {
    ip: string;
    port: int
  } [@@deriving bin_io]

end

module Discovery = struct
  type t = {
    discovery: Discovery_msg.t;
    socket: Luv.UDP.t;
  }

  let create ip port = 
    let socket = Luv.UDP.init () |> Result.get_ok in
    let recv_address = Luv.Sockaddr.ipv4 "0.0.0.0" 6999 |> Result.get_ok in
    Luv.UDP.bind ~reuseaddr:true socket recv_address |> Result.get_ok;
    let _ = 
      Luv.UDP.set_membership socket ~group:"224.100.0.1" ~interface:"0.0.0.0" `JOIN_GROUP |> Result.get_ok in
    {
      discovery={
        ip;
        port
      };
      socket
    }

  let start t fn =
    Luv.UDP.recv_start t.socket begin function
      | Error e -> 
        report_err "Read error: %s" e;
      | Ok (_ , None, _) -> 
        Log.debug (fun m -> m "got ping from")
      | Ok (buffer, Some client, _flags) ->
        let buffer = Luv.Buffer.to_bytes buffer in
        match Binable.from_bytes (module Discovery_msg) buffer with
        | Error e -> Log.warn (fun m -> m "discovery msg err: %s" e)
        | Ok msg -> fn msg client
    end;
    let timer = Luv.Timer.init () |> Result.get_ok in
    Luv.Timer.start timer ~repeat:3000 0 (fun _ ->

        let _, data = Binable.to_bytes (module Discovery_msg) t.discovery in
        let buf = [Luv.Buffer.from_bytes data] in
        let send_addr = Luv.Sockaddr.ipv4 "224.100.0.1" 6999 |> Result.get_ok in
        Luv.UDP.send t.socket buf send_addr @@ function
        |Error e -> report_err "error sending discovery: %s" e
        | _ -> ()

      ) |> Result.get_ok
end


module Make_log(C: CONFIG)(Log: Logs.LOG): B = struct
  include C

  type t = {
    server: Luv.TCP.t;
    discovery: Discovery.t;
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
    discovery = Discovery.create C.server_ip C.server_port;
    clients = Hashtbl.create 100;
    send_ch = Channel.create ()
  }


  let connect (address, port) =
    let client = Luv.TCP.init () |> Result.get_ok in
    let sock_addr = Luv.Sockaddr.ipv4 address port |> Result.get_ok in 
    Luv.TCP.connect client sock_addr (fun e -> 
        match e with
        | Error e ->
          report_err "Connect error: %s" e
        | Ok () -> Luv.Stream.read_start client begin function
            | Error e ->
              report_err "Read error: %s" e;
              Hashtbl.remove state.clients (address, port);
              Luv.Handle.close client ignore
            | _ -> ()
          end;
      );
    client

  let async_send_to_client =
    Luv.Async.init (fun _ ->
        Channel.consume state.send_ch (fun (destination, buf) ->
            match Hashtbl.find_opt state.clients destination with
            | Some client ->
              Luv.Stream.write client [buf] (fun e _ -> 
                  match e with 
                  | Error e -> report_err "Write error: %s" e
                  | Ok () -> ()
                );
            | None -> 
              Log.debug (fun m -> m "client not found");
          );
      ) |> Result.get_ok

  let send destination buf = 
    Channel.send state.send_ch (destination, buf);
    Luv.Async.send async_send_to_client 
    |> Result.iter_error (report_err "Send error: %s")

  let start fn = 
    Luv.Stream.listen state.server begin function
      | Error e ->
        report_err "Listen error: %s" e
      | Ok () ->
        let client = Luv.TCP.init () |> Result.get_ok in

        match Luv.Stream.accept ~server:state.server ~client with
        | Error e ->
          report_err "Accept error: %s" e;
          Luv.Handle.close client ignore
        | Ok () ->
          Luv.Stream.read_start client begin function
            | Error `EOF ->
              Log.debug (fun m -> m "client hangup");
              Luv.Handle.close client ignore
            | Error e ->
              report_err "Read error: %s" e;
              Luv.Handle.close client ignore
            | Ok buffer ->
              fn client buffer
          end;
    end;
    Discovery.start state.discovery 
      (fun msg _sock ->
         let destination = (msg.ip, msg.port) in
         match Hashtbl.find_opt state.clients destination with
         | None -> 
           let client = connect destination in
           Hashtbl.replace state.clients destination client
         | _ -> ()
      )
end

module Make(C: CONFIG) = Make_log(C)(Log)
