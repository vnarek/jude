module Msg = struct
  open Bin_prot.Std

  type digest = string [@@deriving bin_io]

  type pid = string [@@deriving bin_io]

  type t =
    | Syn of (string * int)
    | Ready of {
        source : string * int;
        names : (string * Pid.t) list;
        ack : bool;
      }
    | ToActor of (pid * digest * bytes)
  [@@deriving bin_io]
end

module Msg_exit = struct
  open Bin_prot.Std

  type t = [ `Normal of Pid.t | `Error of string * Pid.t ]
  [@@deriving bin_io]
end

module Resolution_msg = struct
  open Bin_prot.Std

  type t = Success of (string * Pid.t) [@@deriving bin_io]
end
