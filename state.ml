open Player
open Board
open Tile

type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas: (int*int*color) list
}

let init_state = {
  board = Board.init_board;
  players = [Player.init_player "Player 1" Yellow; Player.init_player "Player 2" Blue];
  canvas = [(-1,1,White);  (0,1,White);  (1,1,White);
            (-1,0,White);  (0,0,White);  (1,0,White);
            (-1,-1,White); (0,-1,White); (1,-1,White);]
}

let get_center_cell st pos= let (x,y)= Array.get st.board pos in (x,y)

(* in 3X3 all cells must be WHITE;
*)
let get_select_site st center_cell= failwith "SDF"


(* let is_available_loc board selection_site =
  let board_lst = Array.to_list board in
  let cood = List.find(fun (x', y', c') -> if (x' = x) && (y' = y) && c' = white *)



let is_valid_move = failwith "SDF"

let do' = failwith "SDF"

let print_state = failwith "SDF"
