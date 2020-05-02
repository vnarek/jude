
type 'a m = (module Bin_prot.Binable.S with type t = 'a)

(*let to_bytes (type a) (m: a m) msg = 
  let (module Bin) = m in
  let open Bin_prot in
  let buf = Utils.bin_dump Bin.bin_writer_t msg in
  let bytes = Bytes.create Bin.bin_write_t*)

let to_bytes (type a) (m: a m) msg =
  let (module Bin) = m in
  let size = Bin.bin_size_t msg in
  let buf = Bin_prot.Common.create_buf size in
  let len = Bin.bin_write_t buf ~pos:0 msg in
  let bytes = Bytes.create len in
  Bin_prot.Common.blit_buf_bytes buf bytes ~len;
  bytes

let from_bytes (type a) (m: a m) bytes =
  let (module Bin) = m in
  let len = Bytes.length bytes in
  let buf = Bin_prot.Common.create_buf len in
  Bin_prot.Common.blit_bytes_buf bytes buf ~len;
  Bin.bin_read_t buf ~pos_ref:(ref 0)
