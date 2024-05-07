(lang dune 2.9)
(name effects-examples)
(generate_opam_files true)
(source (github ocaml-multicore/effects-examples))
(authors "KC Sivaramakrishnan <kc@kcsrk.info>")
(maintainers "KC Sivaramakrishnan <kc@kcsrk.info>")
(package
 (name effects-examples)
 (synopsis "A collection of example programs using effects in OCaml 5")
 (description "A collection of example programs using effects in OCaml 5")
 (depends
   (lwt (>= "5.7"))
   (multicont (>= "1.0.1"))))