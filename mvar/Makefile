EXE := chameneos.exe chameneos_monad.exe MVar_test.exe chameneos_systhr.exe \
	chameneos_lwt.exe

all: $(EXE)

MVar_test.exe: MVar_test.ml
	ocamlopt -o MVar_test.exe sched.mli sched.ml MVar.mli MVar.ml MVar_test.ml

chameneos_systhr.exe: chameneos_systhr.ml
	ocamlfind ocamlc -o chameneos-systhr.exe -thread -package threads \
		-linkpkg chameneos_systhr.ml

chameneos_lwt.exe: chameneos_lwt.ml
	ocamlfind ocamlc -o chameneos-lwt.exe -thread -package lwt,lwt.unix,threads \
		-linkpkg chameneos_lwt.ml

chameneos-ghc.exe: chameneos.hs
	ghc -o chameneos-ghc.exe -cpp -XBangPatterns -XScopedTypeVariables \
	-XGeneralizedNewtypeDeriving chameneos.hs

chameneos.exe: chameneos.ml
	ocamlopt -o chameneos.exe sched.mli sched.ml MVar.mli MVar.ml chameneos.ml

chameneos_monad.exe: chameneos_monad.ml
	ocamlopt -o chameneos_monad.exe sched_monad.mli sched_monad.ml \
		MVar_monad.mli MVar_monad.ml chameneos_monad.ml

clean:
	rm -f *.cmi *.cmx a.out *~ eff *.cmo MVar_test chameneos-eff chameneos-ghc \
	*.hi *.o $(EXE)
