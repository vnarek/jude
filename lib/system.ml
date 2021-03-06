module Diff = struct
  open Bin_prot.Std

  type name = string * Pid.t [@@deriving bin_io]

  type t = New of name | Delete of string [@@deriving bin_io]
end

module Msg = struct
  open Bin_prot.Std

  type digest = string [@@deriving bin_io]

  type pid = string [@@deriving bin_io]

  type t =
    | Syn of (string * int)
    | Ready of { source : string * int; names : Diff.name list; ack : bool }
    | Deliver_msg of (pid * digest * bytes)
    | Name_update of Diff.t
  [@@deriving bin_io]
end

module Exit_msg = struct
  open Bin_prot.Std

  type t = [ `Normal of Pid.t | `Error of string * Pid.t ] [@@deriving bin_io]
end

module Signal_msg = struct
  open Bin_prot.Std

  type t = [ `Exception of string * Pid.t ] [@@deriving bin_io]
end

module Resolution_msg = struct
  open Bin_prot.Std

  type t = Success of (string * Pid.t) [@@deriving bin_io]
end
