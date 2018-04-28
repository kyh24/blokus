open Player
open Tile
open Board

(*[state] represents the state of the game*)
type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas: (int*int* color) list
}


(*[init_state] is the initial state of the game with dimensions i by i*)
val init_state : state

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
(* val is_valid_move: state -> int -> bool *)


(* [do' c st t] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
*)
val do' : Command.command -> state -> tile -> state

(*[print_state s] prints a string representation of the state of the game*)
val print_state: state -> unit

(*[flip_tile t dir] is tile t after a reflection over axis specified by [dir]*)
val flip_tile : tile -> Command.direction -> tile

(*[turn_tile t] is tile t after a 90 degree turn clockwise*)
val turn_tile: tile -> tile
