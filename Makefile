compile:
	ocamlbuild -use-ocamlfind state.cmo command.cmo gui.cmo tile.cmo player.cmo board.cmo

clean:
	ocamlbuild -clean

play:
	ocamlbuild -pkgs unix,oUnit,graphics -tag thread main.byte && ./main.byte -gui
