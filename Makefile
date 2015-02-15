# naive Makefile that drives ocamlbuild

byte: 
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 src/main.byte
	@mv main.byte luata

native:
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 src/main.native
	@mv main.native luata

js:
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o src/jsbridge.byte && js_of_ocaml --disable deadcode jsbridge.byte

top:	byte
#	make
#	find _build/src/ -name *.cmo | sed 's/_build\///; s/.cmo//' > src/main.mltop
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 src/main.top

doc:	byte
#	make
#	find _build/src/ -name *.cmo | sed 's/_build\///; s/.cmo//' > src/main.odocl
#	ocamlbuild -yaccflag -v -ocamldoc 'ocamldoc -t "LuaTA API"' src/main.docdir/index.html
	ocamldoc -html -d doc -I _build/src -t "LuaTA API" _build/src/*.mli _build/src/*.ml

test:
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 -use-ocamlfind -package oUnit src/test.byte
	@mv test.byte test

edslcheck:
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 -use-ocamlfind -package qcheck src/edslcheck.byte
	@mv edslcheck.byte edslcheck

testcov:
	rm -f bisect*.out
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 -use-ocamlfind -package oUnit,bisect -pp 'camlp4o str.cma `ocamlfind query bisect`/bisect_pp.cmo -exclude-file ../cov-exclude.txt' src/test.byte

runtestcov:
	./test.byte
	mv bisect0001.out cov-report-test.out

edslcheckcov:
	rm -f bisect*.out
	ocamlbuild -yaccflag -v -cflags -warn-error,+26 -use-ocamlfind -package qcheck,bisect -pp 'camlp4o str.cma `ocamlfind query bisect`/bisect_pp.cmo -exclude-file ../cov-exclude.txt' src/edslcheck.byte

runedslcheckcov:
	./edslcheck.byte
	mv bisect0001.out cov-report-qcheck.out

testreport:
	cd _build/ && bisect-report -html ../cov-report-test ../cov-report-test.out && cd ../
	cd _build/ && bisect-report -csv ../cov-report-test.csv ../cov-report-test.out && cd ../

checkreport:
	bisect-report -I _build -html cov-report-qcheck cov-report-qcheck.out
	bisect-report -I _build -csv cov-report-qcheck.csv cov-report-qcheck.out

combinedreport:
	cd _build/ && bisect-report -html ../cov-report-combined ../cov-report-test.out ../cov-report-qcheck.out && cd ../
	cd _build/ && bisect-report -csv ../cov-report-combined.csv ../cov-report-test.out ../cov-report-qcheck.out && cd ../

clean:
	ocamlbuild -clean
	rm -f src/*~ parser.output doc/*.html doc/style.css luata-out.js
