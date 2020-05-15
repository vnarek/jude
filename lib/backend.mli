module type CONFIG = sig
  val server_ip : string

  val server_port : int
end

module DefaultConfig : CONFIG

module type B = sig
  type conn = Conn.t

  val server_conn : conn

  val start : on_disc:(conn -> unit) -> on_conn:(Luv.Buffer.t -> unit) -> unit

  val send : conn -> Luv.Buffer.t -> unit
end

module Make : functor (_ : CONFIG) -> B
