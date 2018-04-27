Open Player
  Open Tile
  Open Board

(*[state] represents the state of the game*)
type state = {
  board : int*int list;
  players : player list;
  mutable canvas: (int*int* color) list
}
(*[tile] is a Tile representing the color of a player's pieces in the game*

(*[board] is the Board representing the entire 8*8 frame of the game*)
type board= Board

(*[color] is a string representing the color of a player's pieces in the game*)
type color = string

(*[init_state] is the initial state of the game with dimensions i by i*)
val init_state : () -> state

(** [is_valid_move lst] is true if the following conditions are satisfied
 *  (1) none of the edges are touching an edge of the same color
 * 	(2) vertex of tile placed is touching the vertex of at least one
        previously placed tile of the same color
    (3) all coordinates of the tile placed do not already have a tile
    placed there
    (4) all coordinates of the tile placed are valid coordinates on
        board (no part of the tile is off the board)
 *
*)
val is_valid_move: int*int list -> color -> bool


(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
*)
val do' : Command.command -> state -> state

(*[print_state s] prints a string representation of the state of the game*)
val print_state: state -> unit
