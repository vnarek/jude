type error = Digest_mismatch of string * string
type 'a set = ('a, unit) Hashtbl.t

type t = {
  selfPid: Pid.t;
  mailbox: (string * bytes) Mailbox.t;
  mutable cont: Matcher.t ref;
  links: Pid.t set
}

let create pid =
  let mailbox = Mailbox.create () |> Result.get_ok in
  {
    selfPid = pid;
    cont = ref Matcher.sink;
    mailbox = mailbox;
    links = Hashtbl.create 15;
  }

let init fn t  =
  let process = fn t in
  t.cont := process

let receive t digest buf = Mailbox.push t.mailbox (digest, buf)

let step t =
  let o = Mailbox.take t.mailbox
          |> Option.map @@ fun (digest, buf) -> !(t.cont) digest buf in
  Option.value o ~default:(Error "not found")

let selfPid {selfPid; _} = selfPid

let become t fn = t.cont := fn t

let link a b = Hashtbl.add a.links b ()

let link_iter fn a = Hashtbl.iter (fun a _ -> fn a) a.links
