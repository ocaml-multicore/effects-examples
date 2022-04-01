EXE := concurrent.exe state.exe ref.exe transaction.exe echo.exe \
	dyn_wind.exe generator.exe promises.exe reify_reflect.exe \
	MVar_test.exe chameneos.exe eratosthenes.exe pipes.exe loop.exe \
	fringe.exe algorithmic_differentiation.exe

all: $(EXE)

concurrent.exe: sched.mli sched.ml concurrent.ml
	dune build concurrent.exe

echo.exe: aio/aio.mli aio/aio.ml aio/echo.ml
	dune build aio/echo.exe

MVar_test.exe: mvar/MVar_test.ml
	dune build mvar/MVar_test.exe 

chameneos.exe: mvar/chameneos.ml
	dune build mvar/chameneos.exe 

chameneos_systhr.exe: mvar/chameneos_systhr.ml
	dune build mvar/chameneos_systhr.exe

chameneos_lwt.exe: mvar/chameneos_lwt.ml
	dune build mvar/chameneos_lwt.exe

chameneos_monad.exe: mvar/chameneos_monad.ml
	dune build mvar/chameneos_monad.exe

chameneos-ghc.exe: mvar/chameneos.hs
	ghc -o mvar/chameneos-ghc.exe -cpp -XBangPatterns -XScopedTypeVariables \
	-XGeneralizedNewtypeDeriving mvar/chameneos.hs

callback:
	dune build callbacks/callback

%.exe: %.ml
	dune build $@

clean:
	dune clean
	rm -f mvar/*.exe mvar/*.o mvar/*.hi mvar/dune-project
	rm -f *.exe
	rm -rf aio/dune-project

.PHONY: clean
