open Tile

type board= ((int * int) * color) array

(*[init_board brd_size] initializes a board with dimensions brd_size x brd_size*)
val init_board: int -> ((int * int) * color) array

(*[brd_size brd] returns the height of board [brd]*)
val brd_size : board -> int

(*[get_index (x,y) size] returns the array index of coordinate [(x,y)] *)
val get_index : int*int -> int -> int

(*[place_tile_on_brd tile_colors brd] places the tile on [tile_colors] onto [brd] ]*)
val place_tile_on_brd : ((int * int) * 'a) list -> ((int * int) * 'a) array -> unit
