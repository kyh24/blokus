type tile_id = One | Tee | L | X | Z | Tree | Line | Two | Four | Cowgirl | Couch
             | Recliner | Kink | C | Boot | Stairmaster

type color = White | Blue | Yellow

(*[tile] is a type representing a tile in the game*)
type tile = {
  name : tile_id;
  col : color;
  value : int;
  mutable grid: ((int * int) * color) list;
  mutable corners: (int * int) list;
}

(*[direction] represents the axis the player wants flip a tile over*)
type direction = X | Y

(*[init_tile id] is the initial state of tile t*)
val init_tile : tile_id -> color ->  tile

(*[tile_name t] is the name of tile t*)
val tile_name : tile -> tile_id

(*[value t] is the value of tile t*)
val value : tile -> int

(*[color t] is the color of tile t*)
val col : tile -> color

(*[color t] is the grid of tile t*)
val grid: tile -> ((int * int) * color) list

(*[flip_tile t dir] reflects tile [t] over the axis specified by [dir]*)
val flip_tile: tile -> direction -> tile

(*[turn_tile t] rotates tile [t] 90 degrees clockwise*)
val turn_tile: tile -> tile

(*[corners_grid t] is a 5x5 grid that incorporates [t.corners]. this function
  allows the mapping of tile coordinates to board coordinates.*)
val grid_of_corners: tile -> ((int * int) * color) list
