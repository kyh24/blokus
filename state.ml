open Player
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
