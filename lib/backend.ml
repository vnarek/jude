module type CONFIG = sig
  val server_ip : string

  val server_port : int
end

module DefaultConfig : CONFIG = struct
  let server_ip = "127.0.0.1"

  let server_port = 7000
end

module type B = sig
  type conn = Conn.t

  val server_conn : conn

  val start : on_disc:(conn -> unit) -> on_conn:(Luv.Buffer.t -> unit) -> unit

  val send : conn -> Luv.Buffer.t -> unit
end

let default_err = format_of_string "error: %s"

let report_err str e = Logs.err @@ fun m -> m str (Luv.Error.strerror e)

let handle_res ?(on_error = ignore) ?(msg = default_err) fn res =
  match res with
  | Error e ->
      report_err msg e;
      on_error e
  | Ok k -> fn k

module Discovery = struct
  module Msg = struct
    open Bin_prot.Std

    type t = { ip : string; port : int } [@@deriving bin_io]
  end

  module Config = struct
    let port = 6999

    let address = "224.100.0.1"
  end

  type t = { discovery : Msg.t; socket : Luv.UDP.t }

  let create ip port =
    let open Result.Syntax in
    let inet_addr = Unix.string_of_inet_addr Unix.inet_addr_any in
    Luv.UDP.init ()
    >>= (fun socket ->
          Luv.Sockaddr.ipv4 inet_addr Config.port >>= fun recv_address ->
          Luv.UDP.bind ~reuseaddr:true socket recv_address >>= fun _ ->
          Luv.UDP.set_membership socket ~group:Config.address
            ~interface:inet_addr `JOIN_GROUP
          >>= fun _ -> Ok { discovery = Msg.{ ip; port }; socket })
    |> Result.unwrap "discovery create"

  let start t fn =
    Luv.UDP.recv_start t.socket
    @@ handle_res ~msg:"read error: %s" (function
         | _, None, _ -> ()
         | buffer, Some client, _flags ->
             let msg = Binable.from_buffer (module Msg) buffer in
             if msg <> t.discovery then fn msg client);
    let timer = Luv.Timer.init () |> Result.unwrap "discovery start" in
    let send_addr =
      Luv.Sockaddr.ipv4 "224.100.0.1" 6999 |> Result.unwrap "udp sockaddr"
    in
    Luv.Timer.start timer ~repeat:3000 0 (fun _ ->
        let buf = Binable.to_buffer (module Msg) t.discovery in
        Luv.UDP.send t.socket [ buf ] send_addr @@ function
        | Error e -> report_err "error sending discovery: %s" e
        | Ok () -> ())
    |> Result.unwrap "timer start"
end

module Make (C : CONFIG) : B = struct
  include C

  type t = {
    server : Luv.TCP.t;
    discovery : Discovery.t;
    clients : (Conn.t, Luv.TCP.t) Hashtbl.t;
    send_ch : (Conn.t * Luv.Buffer.t) Channel.t;
  }

  type conn = Conn.t

  let server_conn = (C.server_ip, C.server_port)

  let server_address =
    Luv.Sockaddr.ipv4 C.server_ip C.server_port
    |> Result.unwrap "backend server address"

  let create_arbiter () =
    let server = Luv.TCP.init () |> Result.unwrap "arbiter create" in
    let _ = Luv.TCP.bind server @@ server_address in
    server

  let state =
    {
      server = create_arbiter ();
      discovery = Discovery.create C.server_ip C.server_port;
      clients = Hashtbl.create 100;
      send_ch = Channel.create ();
    }

  let connect (address, port) =
    let open Result.Syntax in
    let* client = Luv.TCP.init () in
    let* sock_addr = Luv.Sockaddr.ipv4 address port in
    let _ =
      Luv.TCP.connect client sock_addr
      @@ handle_res ~msg:"connect error: %s" (fun _ ->
             Luv.Stream.read_start client
             @@ handle_res ~msg:"read error: %s"
                  ~on_error:(fun _ ->
                    Log.debug (fun m -> m "read error");
                    Hashtbl.remove state.clients (address, port);
                    Luv.Handle.close client ignore)
                  ignore)
    in
    Ok client

  let async_send_to_client =
    Luv.Async.init (fun _ ->
        Channel.consume state.send_ch (fun (destination, buf) ->
            let client =
              match Hashtbl.find_opt state.clients destination with
              | Some client -> Ok client
              | None -> connect destination
            in
            handle_res ~msg:"async send to client: %s"
              (fun client ->
                Luv.Stream.write client [ buf ] (fun e _ ->
                    match e with
                    | Error e -> report_err "Write error: %s" e
                    | Ok () -> ()))
              client))
    |> Result.unwrap "async send to client"

  let send destination buf =
    Channel.send state.send_ch (destination, buf);
    Luv.Async.send async_send_to_client
    |> handle_res ~msg:"send error: %s" ignore

  let start ~on_disc ~on_conn =
    Luv.Stream.listen state.server
    @@ handle_res ~msg:"listen error: %s" (fun _ ->
           let client =
             Luv.TCP.init () |> Result.unwrap "backend start tcp init"
           in
           Luv.Stream.accept ~server:state.server ~client
           |> handle_res ~msg:"accept error: %s"
                ~on_error:(fun _ -> Luv.Handle.close client ignore)
                (fun _ ->
                  Luv.Stream.read_start client (function
                    | Error `EOF ->
                        Log.debug (fun m -> m "client hangup");
                        Luv.Handle.close client ignore
                    | Error e ->
                        report_err "read error: %s" e;
                        Luv.Handle.close client ignore
                    | Ok buffer -> on_conn buffer)));
    Discovery.start state.discovery (fun msg _sock ->
        let destination = Discovery.Msg.(msg.ip, msg.port) in
        match Hashtbl.find_opt state.clients destination with
        | None ->
            connect destination
            |> handle_res ~msg:"discovery client: %s" (fun client ->
                   Hashtbl.replace state.clients destination client;
                   on_disc destination)
        | _ -> ())
end
