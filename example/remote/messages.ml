open Jude
open Bin_prot.Std

module PingMsg = struct
  type t = Ping of (Pid.t * int) [@@deriving bin_io]
end

module PongMsg = struct
  type t = Pong of (Pid.t * int) [@@deriving bin_io]
end