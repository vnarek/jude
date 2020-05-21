type 'a m = (module Bin_prot.Binable.S with type t = 'a)

val to_bytes : 'a m -> 'a -> string * bytes

val to_buffer : 'a m -> 'a -> Bin_prot.Common.buf

val from_bytes : 'a m -> ?digest:string -> bytes -> ('a, string) result

val from_buffer : 'a m -> Bin_prot.Common.buf -> 'a
