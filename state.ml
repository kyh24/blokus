open Player
<<<<<<< HEAD
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
=======
open Board
open Tile

type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas: (int*int*color) list
}

let init_state = {
  board = init_board;
  players = [init_player "Player 1" Yellow, init_player "Player 2" Blue];
  canvas = [(-1,1,White);  (0,1,White);  (1,1,White);
            (-1,0,White);  (0,0,White);  (1,0,White);
            (-1,-1,White); (0,-1,White); (1,-1,White);]
}

let get_center_cell st pos= let (x,y)= Array.get st.board pos in (x,y)

(* in 3X3 all cells must be WHITE;
*)
let get_select_site st center_cell=
  let tile_structure = [| (-1,1)   ; (0,1)  ; (1,1) ;
                          (-1,0)   ; (0,0)  ; (1,0) ;
                          ( -1,-1) ; (0,-1) ; (1,-1)|] in
  let (cx,cy) = center_cell in
  Array.iter (fun cx cy -> tile_structure

              let is_valid_move = failwith "SDF"
>>>>>>> 20675d94c376fffc9ef4ddc5f8fc1041313173be
