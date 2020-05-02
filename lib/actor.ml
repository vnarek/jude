type ctx = {
  selfPid: Pid.t
}

module type DEF = sig
  type t [@@deriving bin_io]
  val receive: ctx -> t -> unit
end

type 'a def = (module DEF with type t = 'a)

module type INSTANCE = sig
  val receive: Luv.Buffer.t -> unit
  val step: unit -> unit
end

let receive (type a) (t: a def) mailbox buf =
  let (module T) = t in
  print_endline "receive";
  Luv.Buffer.to_bytes buf
  |> Binable.from_bytes (module T)
  |> Result.iter (Mailbox.push mailbox)

let create (type a) pid (t: a def) =
  let (module T) = t in
  let mailbox = Mailbox.create () |> Result.get_ok (*FIXME pls*)in
  let process = T.receive {selfPid = pid} in
  let module M = struct
    let receive buf = receive (module T) mailbox buf
    let step () = 
      Mailbox.take mailbox 
      |> Option.get
      |> process
  end in (module M: INSTANCE)