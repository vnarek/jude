

let () =
  Jude.Arbiter.init ();
  Jude.Arbiter.spawn "buenos" (fun _ -> ());
  Jude.Arbiter.spawn "aires" (fun _ -> ());
  Jude.Arbiter.run ();

