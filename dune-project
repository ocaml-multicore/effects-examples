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
   ; TODO: Change when lwt is released with 5.0.0 support 
   (lwt (>= "5.5.1~alpha-repo"))
   (multicont (>= "1.0.0~rc.2"))))