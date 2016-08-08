default: jack.ml cards.ml
	rm -rf _build
	# rm -f *.o
	# rm -f *.cmx
	# rm -f *.cmi
	ocamlopt -c cards.ml
	ocamlopt -c jack.ml
	ocamlopt -o jack cards.cmx jack.cmx

andrun: default
	./jack
