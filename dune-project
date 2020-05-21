(lang dune 2.6)

(using bisect_ppx 1.0)

(name jude)

(generate_opam_files true)

(package
 (name jude)
 (synopsis "Actor model implemented in OCaml - early alpha")
 (description "Actor model implemented in OCaml - early alpha\n")
 (depends
  (dune
   (= "git+https://github.com/ocaml/dune.git#d29cb8522f0c"))
  (alcotest :with-test)
  (odoc :with-doc)
  luv
  logs))
