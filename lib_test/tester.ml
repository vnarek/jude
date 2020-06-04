let () =
  [ Binable.tests; Registry.tests; Actor.tests ]
  |> List.flatten |> Alcotest.run "Jude"
