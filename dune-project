(lang dune 3.9)
(name durin)
(generate_opam_files true)
(cram enable)

(license BSD3)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist <timmcgil@gmail.com>")
(source (github tmcgilchrist/durin))

(package
 (name durin)
 (tags (dwarf debug))
 (depends
  (ocaml (>= 4.14))
  object
  integers
  (cmdliner :with-test)
  (alcotest :with-test)
  (ocamlformat (and :with-dev-setup (= 0.27.0))))
 (synopsis "A library for reading and writing the DWARF debugging format.")
 (description "A library for reading and writing the DWARF debugging format."))