
type tile_id =
  | One | Square | Xshape | Tee | Line | LilL | BigL
  (* i love foster with all my heart <3 blokaml *)

(*[tile] is a type representing a tile in the game*)
type tile = {
  name : tile_id;
  coordinates : int*int list;
  color: string;
  value : int
}

(*[init_tile n coords c v] is the initial state of tile t*)
val init_tile : string -> int*int list -> string -> int -> tile

(*[tile_name t] is the name of tile t*)
val tile_name : tile -> tile_id

(*[value t] is the value of tile t*)
val value : tile -> int

(*[color t] is the color of tile t*)
val color : tile -> string

(*[flip_tile t dir] is tile t after a reflection over axis specified by [dir]*)
val flip_tile : int*int list -> string -> int*int list

(*[turn_tile t] is tile t after a 90 degree turn clockwise*)
val turn_tile : int*int list -> int*int list
