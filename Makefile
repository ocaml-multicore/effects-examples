all: concurrent generator

concurrent: sched.mli sched.ml concurrent.ml
	ocamlc -o concurrent sched.mli sched.ml concurrent.ml

generator: generator.ml
	ocamlc -o generator generator.ml

clean:
	rm -f *.cmi *.cmo *.o concurrent generator *~ a.out
