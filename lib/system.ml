

module Msg = struct 

  open Bin_prot.Std

  type t =  Syn
         | Ready
         | ToActor of (string * string) [@@deriving bin_io]
end

let msg_to_buffer msg = Core_kernel.Binable.to_string (module Msg) msg
                        |> Luv.Buffer.from_string 

let msg_from_buffer buf = 
  Luv.Buffer.to_string buf
  |> Core_kernel.Binable.of_string (module Msg)