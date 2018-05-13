open Player
open Tile
open Board
open Command

(*[state] represents the state of the game*)
type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas: ((int*int)* color) list;
  mutable curr_player: player;
    mutable game_over : bool
}


(*[init_state] is the initial state of the game with dimensions i by i*)
val init_state : int -> state

(** [is_valid_move lst] is true if the following conditions are satisfied:
 *  (1) none of the edges are touching an edge of the same color
 * 	(2) vertex of tile placed is touching the vertex of at least one
        previously placed tile of the same color
    (3) all coordinates of the tile placed do not already have a tile
    placed there
    (4) all coordinates of the tile placed are valid coordinates on
        board (no part of the tile is off the board)
    (5) if it is a player's first move, the tile they place is on their respective
    corners : top-left corner for player 1 and bottom right corner for player 2
 *
*)
val is_valid_move: player -> state -> (float * float) -> tile -> bool


val place_tile: state -> player-> tile -> (float * float) -> unit

val p2_placed_tiles: tile list ref


(* [do' c st t] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
*)
val do' : command -> (float*float) -> player -> state -> tile -> state


(*[print_state s] prints a string representation of the state of the game*)
val print_state: ((int*int) * color) array -> unit
