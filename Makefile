all: concurrent generator state reify_reflect ref transaction aio \
	delimcc dyn_wind queens memo loop

algorithmic_differentiation: algorithmic_differentiation.ml
	ocamlopt -o algorithmic_differentiation algorithmic_differentiation.ml

concurrent: sched.mli sched.ml concurrent.ml
	ocamlopt -o concurrent sched.mli sched.ml concurrent.ml

queens: queens.ml
	ocamlopt -o queens queens.ml

memo: memo.ml
	ocamlopt -o memo memo.ml

generator: generator.ml
	ocamlopt -o generator generator.ml

state: state.ml
	ocamlopt -o state state.ml

reify_reflect: reify_reflect.ml
	ocamlopt -o reify_reflect reify_reflect.ml

ref: ref.ml
	ocamlopt -o ref ref.ml

transaction: transaction.ml
	ocamlopt -o transaction transaction.ml

aio:
	$(MAKE) -C aio

delimcc: delimcc.ml
	ocamlopt -w "-8" -o delimcc delimcc.ml delimcc_paper_example.ml

dyn_wind: dyn_wind.ml
	ocamlopt -o dyn_wind dyn_wind.ml

loop: loop.ml
	ocamlopt -o loop loop.ml

pipes: pipes.ml
	ocamlopt -o pipes pipes.ml

clean:
	rm -f *.cmx *.cmi *.cmo *.o concurrent generator *~ a.out state reify_reflect ref \
		transaction delimcc dyn_wind queens memo loop pipes
	$(MAKE) -C aio clean

.PHONY: aio clean
