let () = [ Mailbox.tests; Binable.tests ] |> List.flatten |> Alcotest.run "Jude"
