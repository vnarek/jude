module type CONFIG = sig
  val server_ip : string

  val server_port : int
end

module DefaultConfig : CONFIG

module type B = sig
  val server_ip : string

  val server_port : int

  val server_address : Luv.Sockaddr.t

  val start :
    on_disc:(string * int -> unit) ->
    on_tcp:(Luv.TCP.t -> Luv.Buffer.t -> unit) ->
    unit

  val connect : string * int -> Luv.TCP.t

  val send : string * int -> Luv.Buffer.t -> unit
end

module Make : functor (_ : CONFIG) -> B
