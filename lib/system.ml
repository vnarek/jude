

module Msg = struct 

  open Bin_prot.Std

  type t =  Syn
         | Ready
         | ToActor of (string * bytes) [@@deriving bin_io]
end
