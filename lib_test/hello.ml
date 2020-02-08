
let test_hello () = Alcotest.(check string) "should be equal" "Hello Narek" (Jude.Hello.hello "Narek") 


let () = 
  Alcotest.run "Utils"
    [
        ( "string-case", 
          [
              Alcotest.test_case "Hello" `Quick test_hello
          ]
        );
    ]