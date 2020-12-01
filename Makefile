EXE := concurrent.exe state.exe ref.exe transaction.exe echo.exe delimcc.exe \
	dyn_wind.exe generator.exe promises.exe queens.exe reify_reflect.exe \
	MVar_test.exe chameneos.exe memo.exe nondeterminism.exe nim.exe \
	eratosthenes.exe pipes.exe loop.exe clone_is_tricky.exe fringe.exe \
	algorithmic_differentiation.exe

all: $(EXE)

concurrent.exe: sched.mli sched.ml concurrent.ml
	ocamlopt -o concurrent.exe sched.mli sched.ml concurrent.ml

echo.exe: aio/aio.mli aio/aio.ml aio/echo.ml
	$(MAKE) -C aio
	cp aio/echo.native echo.exe

MVar_test.exe: mvar/MVar_test.ml
	$(MAKE) -C mvar MVar_test.exe
	cp mvar/MVar_test.exe .

chameneos.exe: mvar/chameneos.ml
	$(MAKE) -C mvar chameneos.exe
	cp mvar/chameneos.exe .

%.exe: %.ml
	ocamlopt -o $@ $<

clean:
	rm -f *.cmx *.cmi *.cmo *.o concurrent generator *~ $(EXE)
	$(MAKE) -C aio clean
	$(MAKE) -C mvar clean

.PHONY: clean
