open Player
open Board
open Tile
open Command

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

let is_valid_move st pos = failwith "SDF"
(* The 3x3 select site must only have WHITE cells.
   The *)

let flip_tile t dir =
 let old_grid = grid t in
 let new_grid = List.map (fun (x,y,_) ->
 let coord =
   begin
   match dir with
   |X -> List.find (fun (a,b,_) -> a=x && b=(-y)) old_grid
   |Y -> List.find (fun (a,b,_) -> a=(-x) && b=y) old_grid
 end in
 let new_c = match coord with (_,_,c) -> c in
 (x,y,new_c)) old_grid in
 t.grid <- new_grid;
 t


let turn_tile t =
 let old_grid = grid t in
 let new_grid = List.map (fun (x,y,_) ->
 let coord = List.find (fun (a,b,_) -> a= (-y) && b=x) old_grid in
 let new_c = match coord with (_,_,c) -> c in
 (x,y,new_c)) old_grid in
 t.grid <- new_grid;
 t

let update_state t st =
  st.canvas <- t.grid;
  st

let do' c st t= failwith "sdf"

(* let do' c st t =
  match c with
  | FLIP X -> update_state (flip_tile t X) st
  | FLIP Y -> update_state (flip_tile t Y) st
  | TURN t -> update_state (turn_tile t) st
  | PLACE t -> update_state t st

let print_state st = ()
*)
