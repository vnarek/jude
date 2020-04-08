type ctx = {
  selfPid: Pid.t
}

module type Def = sig
  type t [@@deriving bin_io]
  val receive: ctx -> t -> unit
end

type 'a def = (module Def with type t = 'a)

module type Instance = sig
  val receive: Luv.Buffer.t -> unit
  val step: unit -> unit
end

let receive (type a) (t: a def) mailbox buf =
  let (module T) = t in
  print_endline "receive";
  let msg = Luv.Buffer.to_string buf
            |> Core_kernel.Binable.of_string (module T) in
  Mailbox.push mailbox msg

let create (type a) pid (t: a def) =
  let (module T) = t in
  let mailbox = Mailbox.create () |> Result.get_ok (*FIXME pls*)in
  let module M = struct
    let receive buf = receive (module T) mailbox buf
    let step () = 
      Mailbox.take mailbox 
      |> Option.get
      |> T.receive {selfPid = pid}
  end in (module M: Instance)