open Jude

let test_on_name_callback () =
  let reg = Registry.create () in
  let succ = ref false in
  Registry.on_name reg "test" (fun _ -> succ := true);
  Registry.register reg ~local:true "test" @@ Pid.create ("127.0.0.1", 7000);
  Alcotest.(check bool) "should be succ" !succ true

let tests =
  [
    ( "registry",
      [
        ( "on_name callback should be call when name registered",
          `Quick,
          test_on_name_callback );
      ] );
  ]
