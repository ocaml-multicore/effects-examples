#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

value caml_to_c	(value unit) {
	CAMLparam1 (unit);
	printf ("[C] Enter caml_to_c\n");

#ifdef MULTI
	static value c_to_caml_closure;
  static int found = 0;
	if (!found) {
    c_to_caml_closure = caml_get_named_value ("c_to_caml", &found);
    if (!found) {
      printf ("caml_to_c: c_to_caml closure not found\n");
      exit(1);
    }
  }

	printf ("[C] Call c_to_caml\n");
	caml_callback(c_to_caml_closure, Val_unit);
#else
	static value * c_to_caml_closure = NULL;
	if (c_to_caml_closure == NULL)
		c_to_caml_closure = caml_named_value("c_to_caml");

	printf ("[C] Call c_to_caml\n");
	caml_callback(*c_to_caml_closure, Val_unit);
#endif
	printf ("[C] Return from c_to_caml\n");

	printf ("[C] Leave caml_to_c\n");
	CAMLreturn (Val_unit);
}
