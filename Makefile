
	
build:
	ocamlbuild -use-ocamlfind -package compiler-libs.common ppx_lambda.native


test: build
	ocamlc -I lib -ppx ./ppx_lambda.native ./test_ppx_lambda.ml -o ./test_ppx_lambda.native


run: clean test
	./test_ppx_lambda.native


code:
	ocamlc -dsource -I lib -ppx $(PPXBIN) $(PPXTEST) -o lambda


tree:
	ocamlc -dparsetree -I lib -ppx $(PPXBIN) $(PPXTEST) -o lambda


clean:
	rm -f *.cmi *.cmo *.native
	rm -rf _build
