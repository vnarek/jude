

module Msg = struct 
  open Bin_prot.Std

  type digest = string [@@deriving bin_io]
  type pid = string [@@deriving bin_io]

  type t =  Syn
         | Ready
         | ToActor of (pid * digest * bytes) [@@deriving bin_io]
end

module Msg_exit = struct
  open Bin_prot.Std

  type t = [
    |`Normal of Pid.t
    | `Error of string * Pid.t
  ] [@@deriving bin_io]
end
