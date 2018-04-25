
type tile_id =
  (* | One | Square | Xshape | Tee | Line | LilL | BigL *)
  | One | Tee | L | X | Z | Tree | Line
  (* i love foster with all my heart <3 blokaml *)

type color = |White |Blue |Yellow

(*[tile] is a type representing a tile in the game*)
type tile = {
  name : tile_id;
  col : color;
  grid: (int * int * color) array
}

(*[init_tile n coords c v] is the initial state of tile t*)
val init_tile : tile_id -> tile

(*[tile_name t] is the name of tile t*)
val tile_name : tile -> tile_id

(*[value t] is the value of tile t*)
val value : tile -> int

(*[color t] is the color of tile t*)
val color : tile -> color

(*[flip_tile t dir] is tile t after a reflection over axis specified by [dir]*)
val flip_tile : int*int list -> string -> int*int list

(*[turn_tile t] is tile t after a 90 degree turn clockwise*)
val turn_tile : int*int list -> int*int list
