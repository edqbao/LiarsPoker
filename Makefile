test:
	ocamlbuild -r -pkgs oUnit,ANSITerminal,str,unix test_main.byte && ./test_main.byte

play:
	ocamlbuild -r -pkgs oUnit,ANSITerminal,str,unix main.byte && ./main.byte

clean:
	ocamlbuild -clean
	rm -f round.mli.orig deck.mli.orig pokergame.mli.orig