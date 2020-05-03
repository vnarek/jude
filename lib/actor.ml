type ctx = {
  selfPid: Pid.t
}

module type DEF = sig
  type t [@@deriving bin_io]
  val receive: ctx -> t -> unit
end

type 'a def = (module DEF with type t = 'a)

type error = Digest_mismatch

module type INSTANCE = sig
  val receive: digest:string -> bytes -> (unit, error) result
  val step: unit -> unit
end

let receive (type a) (t: a def) mailbox buf =
  let (module T) = t in
  Binable.from_bytes (module T) buf
  |> Result.iter (Mailbox.push mailbox)

let create (type a) pid (t: a def) =
  let (module T) = t in
  let mailbox = Mailbox.create () |> Result.get_ok (*FIXME pls*)in
  let process = T.receive {selfPid = pid} in
  let digest' = Binable.to_digest (module T) in
  let module M = struct
    let receive ~digest buf =
      if digest = digest' then
        Ok(receive (module T) mailbox buf)
      else
        Error Digest_mismatch

    let step () = 
      Mailbox.take mailbox
      |> Option.get
      |> process
  end in (module M: INSTANCE)