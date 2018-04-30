open Player
open Board
open Tile
open Command

type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas: ((int*int)*color) list
}

let init_state s = {
  board = (init_board s);
  players = [Player.init_player "Player 1" Yellow; Player.init_player "Player 2" Blue];
  canvas = [((-1,1),White);  ((0,1),White);  ((1,1),White);
            ((-1,0),White);  ((0,0),White);  ((1,0),White);
            ((-1,-1),White); ((0,-1),White); ((1,-1),White);]
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



(* List.fold_right (fun (x,y,_) acc-> if x < 0 || y < 0 then false::acc else false::acc) select_space [] *)
let check_sides tile_colors brd_array highest_i =
  let tile_colors_ref = ref tile_colors in
  let valid_sides_ref = ref true in
  while (!tile_colors_ref <> [] || !valid_sides_ref) do (
    match !tile_colors_ref with
    | [] -> ()
    | ((x,y),col)::t -> begin
        let board_list = Array.to_list brd_array in
        if (x=0 && y=0) then (
          let right = List.assoc (x+1,y) board_list in
          let bottom = List.assoc (x, y+1) board_list in
          valid_sides_ref := ((right <> col) && (bottom <> col))
        )(*check right and bottom) *)
        else if (x=0 && y=highest_i) then (
          let top = List.assoc (x, y-1) board_list in
          let right = List.assoc (x+1, y) board_list in
          valid_sides_ref := ((top <> col) && (right <> col))
        )(*check top and right*)
        else if (x=highest_i && y=0) then (
          let left = List.assoc (x-1,y) board_list in
          let bottom = List.assoc (x, y+1) board_list in
          valid_sides_ref := ((left <> col) && (bottom <> col))
        ) (*check left and bottom*)
        else if (x=highest_i && y=highest_i) then (
          let top = List.assoc (x, y-1) board_list in
          let left = List.assoc (x-1,y) board_list in
          valid_sides_ref := ((left <> col) && (top <> col))
        ) (*check top and left*)
        else (
          if (x=0) then (
            let top = List.assoc (x, y-1) board_list in
            let right = List.assoc (x+1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            valid_sides_ref := ((top <> col) && (right <> col) && (bottom <> col))
          ) (*don't check the left*)
          else if (x=highest_i) then (
            let top = List.assoc (x, y-1) board_list in
            let left = List.assoc (x-1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            valid_sides_ref := ((top <> col) && (left<> col) && (bottom <> col))
          ) (*dont check right*)
          else if (y=0) then (
            let left = List.assoc (x-1, y) board_list in
            let right = List.assoc (x+1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            valid_sides_ref := ((left <> col) && (right <> col) && (bottom <> col))
          ) (*dont check top*)
          else if (y=highest_i) then (
            let top = List.assoc (x, y-1) board_list in
            let right = List.assoc (x+1, y) board_list in
            let left = List.assoc (x-1, y) board_list in
            valid_sides_ref := ((top <> col) && (right <> col) && (left <> col))
          ) (*don't check bottom*)
          else (
            let top = List.assoc (x, y-1) board_list in
            let right = List.assoc (x+1, y) board_list in
            let left = List.assoc (x-1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            valid_sides_ref := ((top <> col) && (right <> col) && (left <> col)
                                && (bottom <> col))
          ) (*check everything*)
        )
      end )
  done; !valid_sides_ref




let check_corners t brd =
  let brd_lst = Array.to_list brd in
  let brd_coordinates = List.map (fun ((x,y), col) -> (x,y)) brd_lst in
  let valid_corners =  ref (List.filter (fun (x, y) -> List.mem (x,y) brd_coordinates) t.corners) in
  let valid_corners_ref = ref true in
  while (!valid_corners <> [] || !valid_corners_ref) do (
    match !valid_corners with
    |[] -> ()
    |(x,y)::tl -> begin
        let brd_color = List.assoc (x,y) brd_lst in
        if (brd_color = t.col) || (brd_color = White) then valid_corners_ref := true
        else valid_corners_ref := false
      end
  )
  done; !valid_corners_ref

let valid_first_move p tile_colors brd highest_i=
  let brd_lst = Array.to_list brd in
  let valid_brd_coord =  List.filter (fun ((x, y), col) -> List.mem_assoc (x,y) brd_lst) tile_colors in
  let invalid_brd_coord = ref (List.fold_left (fun acc ((x,y),col) ->
      if List.mem_assoc (x,y) valid_brd_coord then acc else ((x,y),col)::acc) [] tile_colors) in
  if (p.first_turn) then (
      let invalid_coord_no_color = ref true in
      while (!invalid_brd_coord <> [] && !invalid_coord_no_color) do(
        match !invalid_brd_coord with
        |[] -> ()
        |((x,y), col)::t -> if col = White then invalid_coord_no_color:= true
          else invalid_coord_no_color := false
      )
      done;
      if (p.name = "Player 1") then
        (List.mem_assoc (0,0) tile_colors && List.assoc (0,0) tile_colors = Yellow) && !invalid_coord_no_color
      else (List.mem_assoc (highest_i, highest_i) tile_colors && List.assoc (highest_i,highest_i) tile_colors = Blue) && !invalid_coord_no_color
  )
  else (List.length !invalid_brd_coord) <> 0


let is_valid_move p st pos tl =
  let highest_i = brd_size st.board in
  let dot = get_center_cell st pos in
  let coordinates = get_selection_space_coords dot in
  let colors_on_board = get_board_colors st.board coordinates [] in
  let colors_of_tile = get_tile_colors tl coordinates [] in
  if (valid_first_move p colors_of_tile st.board highest_i) then (
    let valid_cells = List.map2 (fun ((bx,by),b_col) ((tx,ty), t_col) ->
        if (b_col = White && t_col <> White) then (check_sides colors_of_tile st.board highest_i) && (check_corners tl st.board)
        else t_col = White) colors_on_board colors_of_tile in
    not(List.mem false valid_cells)
  )
  else false
(* The 3x3 select site must only have WHITE cells.
   The *)

let col_to_name col =
  match col with
  |Blue -> "B"
  |Yellow -> "Y"
  |White -> "W"


let update_state t st =
  st.canvas <- t.grid;
  st


let do' c st t = 
  match c with
  | FLIP X -> update_state (flip_tile t X) st
  | FLIP Y -> update_state (flip_tile t Y) st
  | TURN t -> update_state (turn_tile t) st
  | PLACE t -> update_state t st

let rec print_state brd =
let brd_lst = Array.to_list brd in
let str =
 match brd_lst with
 |[] -> ""
 |((x,_),col)::t -> if x <> 7 then col_to_name col ^ " " else col_to_name col ^ " NEXT ROW:"
in print_string str