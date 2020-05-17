open Jude
open Bin_prot.Std

module Ping_msg = struct
  type t = Ping of (Pid.t * int) [@@deriving bin_io]
end

module Pong_msg = struct
  type t = Pong of (Pid.t * int) [@@deriving bin_io]
end
