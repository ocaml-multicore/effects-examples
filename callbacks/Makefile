EXE=callback

all: $(EXE)

callback: foo.c bar.mli bar.ml
	ocamlc -c foo.c
	ocamlc -c bar.mli
	ocamlc -custom -o callback foo.o bar.ml -linkall

clean:
	rm -f *~ *.o *.cmo *.cmi *.cds $(EXE)
