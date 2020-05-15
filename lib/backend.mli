module type CONFIG = sig
  val server_ip : string

  val server_port : int
end

module DefaultConfig : CONFIG

module type B = sig
  type conn = string * int

  val server_conn : conn

  val start :
    on_disc:(string * int -> unit) -> on_conn:(Luv.Buffer.t -> unit) -> unit

  val send : string * int -> Luv.Buffer.t -> unit
end

module Make : functor (_ : CONFIG) -> B
