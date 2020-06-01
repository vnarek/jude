let () =
  [ Binable.tests; Registry.tests; Actor.tests; Mailbox.tests ]
  |> List.flatten |> Alcotest.run "Jude"
