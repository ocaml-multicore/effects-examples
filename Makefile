all:
	ocamlc -o concurrent sched.mli sched.ml concurrent.ml

clean:
	rm -f *.cmi *.cmo *.o concurrent *~
