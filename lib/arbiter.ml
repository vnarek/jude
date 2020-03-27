module Handshake = struct 
  open Bin_prot.Std

  type t =  Syn of string | Ready [@@deriving bin_io]
end
let handshake_to_buffer msg = 
  Core_kernel.Binable.to_string (module Handshake) msg
  |> Luv.Buffer.from_string

let handshake_from_buffer buf = 
  Luv.Buffer.to_string buf
  |> Core_kernel.Binable.of_string (module Handshake)

let arb = Backend.create_arbiter ()


let init () =
  Backend.listen arb (fun conn buf ->
      match handshake_from_buffer buf with
      | Handshake.Syn(name) -> 
        print_endline ("got " ^ name);
        let buf = handshake_to_buffer Handshake.Ready in
        Luv.Stream.write conn [buf] (fun _ _ -> ());
      | _ -> print_endline "nononoe";
    )

let run () = Luv.Loop.run () |> ignore

let handshake name fn client =
  let buf = handshake_to_buffer (Handshake.Syn name) in 
  Luv.Stream.write client [buf] (fun _ _ -> ());
  Luv.Stream.read_start client begin function
    | Error `EOF ->
      Luv.Handle.close client ignore
    | Error e ->
      Printf.eprintf "Read error: %s\n" (Luv.Error.strerror e);
      Luv.Handle.close client ignore
    | Ok res -> match handshake_from_buffer res with
      | Ready -> 
        print_endline "handshake completed";
        (fn client)
      | _ -> print_endline "watafak happened"
  end


let spawn name fn = 
  let client = Backend.create_client () in
  Backend.connect client (handshake name fn);




