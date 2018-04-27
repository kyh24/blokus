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

let get_select_site center_cell=
  let tile_structure = [(-1,-1)   ; (0,-1)  ; (1,-1) ;
                        (-1,0)   ; (0,0)  ; (1,0) ;
                        ( -1,1) ; (0,1) ; (1,1)] in
  let (cx,cy) = center_cell in
  List.map (fun (x,y) -> (x+cx , y+cy) ) tile_structure ;;

let is_valid_move st pos center_cell = failwith "SDF"
(* The 3x3 select site must only have WHITE cells.
  The *)
