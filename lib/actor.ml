type ctx = {
  selfPid: Pid.t
}

type error = Digest_mismatch of string * string

type t = {
  mailbox: (string * bytes) Mailbox.t;
  mutable cont: Matcher.t ref;
}

let create () =
  let mailbox = Mailbox.create () |> Result.get_ok in
  {
    cont = ref Matcher.sink;
    mailbox = mailbox
  }

let init fn t pid  =
  let process = fn {selfPid = pid} in
  t.cont := process

let receive t digest buf = Mailbox.push t.mailbox (digest, buf)

let step t =
  let o = Mailbox.take t.mailbox
          |> Option.map @@ fun (digest, buf) -> !(t.cont) digest buf in
  Option.value o ~default:(Error "not found")

