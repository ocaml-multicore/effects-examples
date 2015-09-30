EXE=callback
COMP=SERIAL

all: $(EXE)

callback: foo.c bar.mli bar.ml
	ocamlc -ccopt -D$(COMP) -c foo.c
	ocamlc -c bar.mli
	ocamlc -custom -o callback foo.o bar.ml -linkall

clean:
	rm -f *~ *.o *.cmo *.cmi *.cds $(EXE)
