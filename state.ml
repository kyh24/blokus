open Player
open Tile

(*[state] represents the state of the game*)

type state = {
  board : int*int list;
  players : player list
}

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


(*[init_state] is the initial state of the game with dimensions i by i*)
let init_state () = failwith "Unimplemented"

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
let is_valid_move lst c = failwith "Unimplemented"

(*[available_locs s] is an x*y list of the coordinates in the board where a tile
  has not been placed*)
let available_locs st = failwith "Unimplemented"

(*[taken_locs s] is an int*int list of the coordinates (x,y) in the board where
  a tile has been placed*)
let taken_locs st = failwith "Unimplemented"

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
*)
let do' c st = failwith "Unimplemented"

(*[print_state s] prints a string representation of the state of the game*)
let print_state st = failwith "Unimplemented"
