type 'a m = (module Bin_prot.Binable.S with type t = 'a)

let hash_length = 8

let eval_to_digest_string shape =
  let digest =
    Bin_prot.Shape.eval_to_digest shape
    |> Bin_prot.Shape.Digest.to_md5 |> Md5_lib.to_binary
  in
  String.sub digest 0 hash_length

let to_bytes (type a) ((module Bin) : a m) msg =
  let size = Bin.bin_size_t msg in
  let buf = Bin_prot.Common.create_buf size in
  let len = Bin.bin_write_t buf ~pos:0 msg in
  let bytes = Bytes.create len in
  Bin_prot.Common.blit_buf_bytes buf bytes ~len;
  (eval_to_digest_string Bin.bin_shape_t, bytes)

let to_buffer (type a) ((module Bin) : a m) msg =
  let size = Bin.bin_size_t msg in
  let buf = Bin_prot.Common.create_buf size in
  let _ = Bin.bin_write_t buf ~pos:0 msg in
  buf

let to_digest (type a) ((module Bin) : a m) =
  Bin_prot.Shape.eval_to_digest_string Bin.bin_shape_t

let check_digest shape digest =
  let digest' = eval_to_digest_string shape in
  if digest = digest' then Ok () else Error "digest failed"

let from_bytes (type a) ((module Bin) : a m) ?digest bytes =
  Option.fold ~none:(Ok ()) ~some:(check_digest Bin.bin_shape_t) digest
  |> Result.map (fun _ ->
         let len = Bytes.length bytes in
         let buf = Bin_prot.Common.create_buf len in
         Bin_prot.Common.blit_bytes_buf bytes buf ~len;
         Bin.bin_read_t buf ~pos_ref:(ref 0))

let from_buffer (type a) ((module Bin) : a m) ?digest arr =
  Option.fold ~none:(Ok ()) ~some:(check_digest Bin.bin_shape_t) digest
  |> Result.map (fun _ -> Bin.bin_read_t arr ~pos_ref:(ref 0))
