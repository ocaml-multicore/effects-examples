all: concurrent generator state reify_reflect ref transaction aio \
	delimcc dyn_wind

concurrent: sched.mli sched.ml concurrent.ml
	ocamlc -o concurrent sched.mli sched.ml concurrent.ml

generator: generator.ml
	ocamlc -o generator generator.ml

state: state.ml
	ocamlc -o state state.ml

reify_reflect: reify_reflect.ml
	ocamlc -o reify_reflect reify_reflect.ml

ref: ref.ml
	ocamlc -o ref ref.ml

transaction: transaction.ml
	ocamlc -o transaction transaction.ml

aio: aio.ml
	ocamlc -o aio unix.cma aio.ml

delimcc: delimcc.ml
	ocamlc -w "-8" -o delimcc delimcc.ml delimcc_paper_example.ml

dyn_wind: dyn_wind.ml
	ocamlc -o dyn_wind dyn_wind.ml

clean:
	rm -f *.cmi *.cmo *.o concurrent generator *~ a.out state reify_reflect ref \
		transaction aio delimcc dyn_wind
