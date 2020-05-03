type 'a m = (module Bin_prot.Binable.S with type t = 'a)

let to_bytes (type a) (m: a m) msg =
  let (module Bin) = m in
  let size = Bin.bin_size_t msg in
  let buf = Bin_prot.Common.create_buf size in
  let len = Bin.bin_write_t buf ~pos:0 msg in
  let bytes = Bytes.create len in
  Bin_prot.Common.blit_buf_bytes buf bytes ~len;
  (Bin_prot.Shape.eval_to_digest_string Bin.bin_shape_t, bytes)

let to_digest (type a) (m: a m) =
  let (module Bin) = m in
  Bin_prot.Shape.eval_to_digest_string Bin.bin_shape_t

let check_digest shape digest =
  let digest' = Bin_prot.Shape.eval_to_digest_string shape in
  if digest = digest' then
    Ok ()
  else
    Error "digest failed"


let from_bytes (type a) (m: a m) ?digest bytes  =
  let (module Bin) = m in
  Option.fold ~none:(Ok()) ~some:(check_digest Bin.bin_shape_t) digest
  |> Result.map (fun _ ->
      let len = Bytes.length bytes in
      let buf = Bin_prot.Common.create_buf len in
      Bin_prot.Common.blit_bytes_buf bytes buf ~len;
      Bin.bin_read_t buf ~pos_ref:(ref 0)
    )


