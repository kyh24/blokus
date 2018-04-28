compile:
	ocamlbuild -use-ocamlfind state.cmo command.cmo gui.cmo tile.cmo player.cmo board.cmo

clean:
	ocamlbuild -clean

play:
	ocamlbuild -use-ocamlfind -pkg graphics main.byte && ./main.byte
	# ocamlbuild -use-ocamlfind main.byte && ./main.byte
# ocamlbuild -pkgs oUnit,yojson,str,graphics -tag thread main.byte && ./main.byte
