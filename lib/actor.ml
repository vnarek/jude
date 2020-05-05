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

let create (type a) (t: a def) =
  let (module T) = t in
  let mailbox = Mailbox.create () |> Result.get_ok in
  {
    cont = ref Matcher.sink;
    mailbox = mailbox
  }

let init (type a) ((module T): a def) t pid  =
  let process = T.receive {selfPid = pid} in
  t.cont := process

let receive t digest buf = Mailbox.push t.mailbox (digest, buf)

let step t =
  let o = Mailbox.take t.mailbox
          |> Option.map @@ fun (digest, buf) -> !(t.cont) digest buf in
  Option.value o ~default:(Error "not found")

