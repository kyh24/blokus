Open Player
Open Tile

(*[state] is an abstract type representing the state of the game*)
type state

(*[player] is a Player representing a player's pieces in the game*)
type player = Player

(*[tile] is a Tile representing the color of a player's pieces in the game*)
type tile = Tile

(*[color] is a string representing the color of a player's pieces in the game*)
type color = string

(** [tile_id] is a variant representing one of the following possible tiles
   One: X      Square: X X       Xshape:   X         Tee:   X X X
                       X X               X X X                X
                                           x                  X

   Line:  X X X X       LilL:  X        BigL:  X
                               X X             X
                                               X X X

*)
type tile_id =
  | One | Square | Xshape | Tee | Line | LilL | BigL
  (* i love foster with all my heart <3 blokaml *)

(*[init_state i] is the initial state of the game with dimensions i by i*)
val init_state : int -> state

(** [is_valid_move lst] is true if the following conditions are satisfied
 *   (1) none of the edges are touching an edge of the same color
 * 	(2) vertex of tile placed is touching the vertex of at least one
        previously placed tile of the same color
    (3) all coordinates of the tile placed do not already have a tile
    placed there
    (4) all coordinates of the tile placed are valid coordinates on
        board (no part of the tile is off the board)
 *
*)
val is_valid_move: int*int list -> color -> bool

(*[available_locs s] is an x*y list of the coordinates in the board where a tile
has not been placed*)
val available_locs: state -> int*int list

(*[taken_locs s] is an int*int list of the coordinates (x,y) in the board where
  a tile has been placed*)
val taken_locs: state -> int*int list

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
*)
val do' : Command.command -> state -> state

(*[print_state s] prints a string representation of the state of the game*)
val print_state: state -> unit
