open Jude

module Backend = Backend.Make(struct
    let server_ip = "127.0.0.1"
    let server_port = 7000
  end)

module Arbiter = Arbiter.Make(Backend)


let () =
  Logs.Src.set_level Jude.Log.log_src (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());

  Arbiter.init();
  Arbiter.run()