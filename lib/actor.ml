type error = Digest_mismatch of string * string
type 'a set = ('a, unit) Hashtbl.t

type process_flags = [`Trap_exit]

type t = {
  selfPid: Pid.t;
  mailbox: (string * bytes) Mailbox.t;
  mutable cont: Matcher.t ref;
  links: Pid.t set;
  monitors: Pid.t set;
  flags: process_flags set
}

type beh = t -> Matcher.t

let create pid =
  let mailbox = Mailbox.create () |> Result.get_ok in
  {
    selfPid = pid;
    cont = ref Matcher.block;
    mailbox = mailbox;
    links = Hashtbl.create 15;
    monitors = Hashtbl.create 15;
    flags = Hashtbl.create 1;
  }

let init fn t  =
  let process = fn t in
  t.cont := process

let receive t digest buf = Mailbox.push t.mailbox (digest, buf)

let step t =
  Mailbox.filter t.mailbox (fun (digest, buf) ->
      let r = !(t.cont) digest buf in 
      Result.iter_error (fun r -> Log.debug (fun m -> m "j %s" r)) r;
      Result.is_error r (* Could starve the connection *)
    )

let selfPid {selfPid; _} = selfPid

let become t fn = t.cont := fn t

let link a b = Hashtbl.add a.links b ()

let link_iter fn a = Hashtbl.iter (fun a _ -> fn a) a.links

let monitor a b = Hashtbl.add b.monitors a ()

let unmonitor a b = Hashtbl.remove b.monitors a

let monitor_iter fn a = Hashtbl.iter (fun a _ -> fn a) a.monitors

let set_flag t p =
  Hashtbl.replace t.flags p ()

let unset_flag t p = Hashtbl.remove t.flags p

let has_flag t p =
  Hashtbl.find_opt t.flags p
  |> Option.is_some
