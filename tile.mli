
(*[tile] is a type representing a tile in the game*)
type tile = {
  name : string;
  coordinates : int*int list;
  color: string;
  value : int
}

(*[init_tile n coords c v] is initial state of tile t*)
val init_tile : string -> int*int list -> string -> int -> tile

(*[tile_name t] is the name of tile t*)
val tile_name : tile -> string

(*[value t] is the value of tile t*)
val value : tile -> int

(*[color t] is the color of tile t*)
val color : tile -> string

(*[flip_tile t] is tile t after a *)
val flip_tile : int*int list -> int*int list

val turn_tile : int*int list -> int*int list
