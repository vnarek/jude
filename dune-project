(lang dune 2.4)

(name jude)

(generate_opam_files true)

(package
 (name jude)
 (synopsis "Actor model implemented in OCaml - early alpha")
 (description "Actor model implemented in OCaml - early alpha\n")
 (maintainers "Narek Vardanjan")
 (authors "Narek Vardanjan")
 (homepage "https://github.com/vnarek/jude")
 (bug_reports "https://github.com/vnarek/jude/issues")
 (depends
  (alcotest :with-test)
  (odoc :with-doc)
  ocamlformat
  luv
  logs))
