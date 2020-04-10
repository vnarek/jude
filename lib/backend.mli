module type CONFIG = sig 
  val server_ip : string 
  val server_port : int 
end

module DefaultConfig : CONFIG

module type B = sig
  type t = Luv.TCP.t

  val server_ip : string
  val server_port : int
  val server_address : Luv.Sockaddr.t
  val server_address_s : string
  val create_arbiter : unit -> Luv.TCP.t
  val listen : Luv.TCP.t -> (Luv.TCP.t -> Luv__.Buffer.t -> unit) -> unit
  val connect : (string * int) -> (Luv.TCP.t -> unit) -> Luv.TCP.t
end

module Make : functor (C : CONFIG) -> B