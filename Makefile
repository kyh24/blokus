test:
	ocamlbuild -pkgs oUnit,graphics -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -use-ocamlfind state.cmo command.cmo gui.cmo tile.cmo player.cmo board.cmo

clean:
	ocamlbuild -clean

play:
	ocamlbuild -pkgs unix,graphics -tag thread main.byte && ./main.byte -gui
