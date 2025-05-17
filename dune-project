(lang dune 3.0)
(name durin)
(generate_opam_files true)

(license BSD3)

(package
 (name durin)
 (depends
  (ocaml (>= 4.14))
  object
  (cmdliner :with-test))
 (synopsis "A library for reading and writing the DWARF debugging format.")
 (description "A library for reading and writing the DWARF debugging format."))