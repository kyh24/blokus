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

let get_center_cell st pos= fst (Array.get st.board pos)

let get_selection_space_coords center_cell=
  let cx = fst center_cell in
  let cy = snd center_cell in
  [(cx -1,cy -1); (cx, cy-1); (cx+1,cy-1);
   (cx-1,cy)    ; (cx,cy   ); (cx+1,cy  );
   (cx-1,cy+1)  ; (cx,cy+1 ); (cx+1,cy+1)]

let rec get_board_colors brd coords acc =
  match coords with
  | [] -> acc
  | (x,y)::t -> begin
      let lst_of_board = brd |> Array.to_list in
      let colors = List.assoc (x,y) lst_of_board in
      get_board_colors brd t (((x,y),colors)::acc)
    end

let rec get_tile_colors tl coords acc =
  match coords with
  | [] -> acc
  | h::t -> begin
      let colors = List.assoc h tl.grid in
      get_tile_colors tl t ((h,colors)::acc)
    end


(*let valid_first_move p select_space =
  if (p.name = "Player 1" && match select_space.get (0,0) with (_,_,c) -> c = Yellow)) ||*)

(*[is_valid_select space t select_space] is true if none of the coordinates in
  [select_space] are negative, unless it is the player's first move. false
  otherwise.*)
(*let is_valid_select_space p select_space =
  if p.first_turn = true then (
    let lst = List.map (fun (x,y,c) -> if ((x < 0 || x> 7) || (y<0 || y > 7))
                         then (if c <> White then false else true) else true) select_space in
    (not (List.mem false lst)) &&  else (
    true)*)


(* List.fold_right (fun (x,y,_) acc-> if x < 0 || y < 0 then false::acc else false::acc) select_space [] *)
let check_sides = if (5 > 4) then true else false
let check_corners = true

let is_valid_move st pos tl =
  let dot = get_center_cell st pos in
  let coordinates = get_selection_space_coords dot in
  let colors_on_board = get_board_colors st.board coordinates [] in
  let colors_of_tile = get_tile_colors tl coordinates [] in
  List.map2 (fun ((bx,by),b_col) ((tx,ty), t_col) ->
      if (b_col = White && t_col <> White) then check_sides && check_corners
      else t_col = White && check_corners) colors_on_board colors_of_tile
(* The 3x3 select site must only have WHITE cells.
   The *)

let print_state st = ()

(*let update_state t st =
  st.canvas <- t.grid;
  st*)

let do' c st t= failwith "sdf"

(* let do' c st t =
   match c with
   | FLIP X -> update_state (flip_tile t X) st
   | FLIP Y -> update_state (flip_tile t Y) st
   | TURN t -> update_state (turn_tile t) st
   | PLACE t -> update_state t st

   let print_state st = ()
*)
