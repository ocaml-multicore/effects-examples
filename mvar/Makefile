all: MVar_test chameneos

chameneos: chameneos-ghc chameneos-eff

#Will only work with vanilla ocaml
chameneos-systhr: chameneos_systhr.ml
	ocamlfind ocamlc -o chameneos-systhr -thread -package threads -linkpkg chameneos_systhr.ml

#Will only work with vanilla ocaml
chameneos-lwt: chameneos_lwt.ml
	ocamlfind ocamlc -o chameneos-lwt -thread -package lwt,lwt.unix,threads -linkpkg chameneos_lwt.ml


chameneos-ghc: chameneos.hs
	ghc -o chameneos-ghc -cpp -XBangPatterns -XScopedTypeVariables \
	-XGeneralizedNewtypeDeriving chameneos.hs

#Needs multicore branch
chameneos-eff: chameneos.ml
	ocamlc -o chameneos-eff sched.mli sched.ml MVar.mli MVar.ml chameneos.ml

#Needs multicore branch
MVar_test:
	ocamlc -o MVar_test sched.mli sched.ml MVar.mli MVar.ml MVar_test.ml

clean:
	rm -f *.cmi *.cmx a.out *~ eff *.cmo MVar_test chameneos-eff chameneos-ghc \
	*.hi *.o chameneos-systhr chameneos-lwt *.annot
