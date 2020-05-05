type ctx = {
  selfPid: Pid.t
}

module type DEF = sig
  type t [@@deriving bin_io]
  val receive: ctx -> Matcher.t
end

type 'a def = (module DEF with type t = 'a)

type error = Digest_mismatch of string * string

type t = {
  mailbox: (string * bytes) Mailbox.t;
  mutable cont: Matcher.t ref;
}

module type INSTANCE = sig
  val receive: digest:string -> bytes -> unit
  val step: unit -> (unit, string) result
end

let receive (type a) (t: a def) mailbox buf =
  let (module T) = t in
  Binable.from_bytes (module T) buf
  |> Result.iter (Mailbox.push mailbox)

let create (type a) (t: a def) =
  let (module T) = t in
  let mailbox = Mailbox.create () |> Result.get_ok in
  let t = {
    cont = ref Matcher.sink;
    mailbox = mailbox
  } in
  let module M = struct
    let receive ~digest buf = Mailbox.push mailbox (digest, buf)

    let step () = 
      let o = Mailbox.take mailbox
              |> Option.map @@ fun (digest, buf) -> !(t.cont) digest buf in
      Option.value o ~default:(Error "not found")
  end in (t, (module M: INSTANCE))

let init (type a) ((module T): a def) t pid  =
  let process = T.receive {selfPid = pid} in
  t.cont := process

