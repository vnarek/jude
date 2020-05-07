

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

  type t = [`Normal
           | `Exception of string
           | `IllegalState of string
           ] [@@deriving bin_io]
end