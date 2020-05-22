module type SERVER_CONFIG = sig
  type t = { ip : string; port : int } [@@deriving bin_io]
end

module Server_config : SERVER_CONFIG

module type CONFIG = sig
  val server : Server_config.t

  val discovery : Server_config.t
end

val create_config :
  ?server:Server_config.t ->
  ?discovery:Server_config.t ->
  unit ->
  (module CONFIG)

module type B = sig
  type conn = Conn.t

  val server_conn : conn

  val start : on_disc:(conn -> unit) -> on_conn:(Luv.Buffer.t -> unit) -> unit

  val send : conn -> Luv.Buffer.t -> unit

  val send_all : Luv.Buffer.t -> unit
end

module Make : functor (_ : CONFIG) -> B
