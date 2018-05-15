open Player
open Board
open Tile
open Command
open Array
open List

type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas1: ((int*int)*color) list;
  mutable canvas2: ((int*int)*color) list;
  mutable curr_player: player;
  mutable game_over : bool;
}

let empty_grid = [((-1,1),White);  ((0,1),White);  ((1,1),White);
                  ((-1,0),White);  ((0,0),White);  ((1,0),White);
                  ((-1,-1),White); ((0,-1),White); ((1,-1),White)]

let init_state s =
  let player_1 = init_player "Player 1" Yellow in
  let player_2 = init_player "Player 2" Blue in
  {
  board = (init_board s);
  players = [player_1; player_2];
  canvas1 = empty_grid;
  canvas2 = empty_grid;
  curr_player = player_1;
  game_over = false
}

let get_center_cell st (x,y) =
  let brd_coord = (x - 400) / 25 , -(((y - 175) / 25) - 15) in
    let index = get_index brd_coord (brd_size st.board) in
    fst (get st.board index)

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
      let lst_of_board = brd |> to_list in
      if (mem_assoc (x,y) lst_of_board) then (
        let colors = lst_of_board |> assoc (x,y) in
        (((x,y),colors)::acc) |> get_board_colors brd t
      ) else acc |> get_board_colors brd t
    end

(* [convert_to_board_coors (x,y) grid] converts the coordinates in
   [grid] to their corresponding board coordinates based on the center cell
   (x,y) *)
let convert_to_board_coors (x,y) grid =
  fold_left (fun acc ((g_x,g_y),col) -> (((x+ g_x),(y - g_y)),col)::acc) [] grid

(* [corners_grid_to_board_coords (x,y) c_grid t_to_b max_i] converts the
   coordinates in [c_grid]to their corresponding board coordinates based on
   the center cell (x,y). in other words, it returns a list of the corners'
   coordinates and colors. then, filters out any of the coordinates that are
   1) out of bounds of the dimensions of the board
   2) part of the tile itself
   3) colored white*)
let corners_grid_to_board_coords (x,y) t t_to_b max_i =
  let c_grid = grid_of_corners t in
  let new_c_grid = c_grid |> convert_to_board_coors (x,y) in
  let remove_whites = filter (fun ((x,y),c) -> (c = White) = false)
      new_c_grid in
  let remove_out_of_bounds = filter (fun ((x,y),c) ->
      x >= 0 && x <= max_i && y >= 0 && y <= max_i)
      remove_whites in
  filter (fun t_coor -> (mem t_coor t_to_b) = false) remove_out_of_bounds

(* [tl_b_coords b_cols acc] is a list of the coordinates mapped to their colors
    when the tile coordinates are mapped to board cooordinates. In other words, color
    does not change but the tile coordinates are in terms of the board. *)
let rec get_tile_colors tl_b_coords b_cols acc =
  match b_cols with
  | [] -> acc
  | (coord,col)::t -> begin
      let colors = assoc coord tl_b_coords in
      get_tile_colors tl_b_coords t ((coord,colors)::acc)
    end

(* [extract_t_cols_only t_grid_cols] is the list representing the tile itself,
   not the entire canvas that includes white spaces. *)
let extract_t_cols_only t_grid_cols =
  filter (fun ((x,y),col) -> col != White) t_grid_cols

(* [check_sides t_cols_ref brd_array highest_i is_valid_ref]
    is true if the tile to be placed/played is to be placed in a space that does not result in
    any of the sides of tile touching the sides of other tiles on the board of the same color. *)
let check_sides t_cols_ref brd_array highest_i is_valid_ref =
  while ((!t_cols_ref <> []) && !is_valid_ref) do (
    match !t_cols_ref with
    | [] -> ()
    | ((x,y),col)::t -> begin
        let board_list = to_list brd_array in
        if (x=0 && y=0) then (
          let right = assoc (x+1,y) board_list in
          let bottom = assoc (x, y+1) board_list in
          is_valid_ref := ((right <> col) && (bottom <> col));
          t_cols_ref := t;
        )
        else if (x=0 && y=highest_i) then (
          let top = assoc (x, y-1) board_list in
          let right = assoc (x+1, y) board_list in
          is_valid_ref := ((top <> col) && (right <> col));
          t_cols_ref := t;
        )
        else if (x=highest_i && y=0) then (
          let left = assoc (x-1,y) board_list in
          let bottom = assoc (x, y+1) board_list in
          is_valid_ref := ((left <> col) && (bottom <> col));
          t_cols_ref := t;
        )
        else if (x=highest_i && y=highest_i) then (
          let top = assoc (x, y-1) board_list in
          let left = assoc (x-1,y) board_list in
          is_valid_ref := ((left <> col) && (top <> col));
          t_cols_ref := t;
        )
        else (
          if (x=0) then (
            let top = assoc (x, y-1) board_list in
            let right = assoc (x+1, y) board_list in
            let bottom = assoc (x, y+1) board_list in
            is_valid_ref := ((top <> col) && (right <> col) && (bottom <> col));
            t_cols_ref := t;
          )
          else if (x=highest_i) then (
            let top = assoc (x, y-1) board_list in
            let left = assoc (x-1, y) board_list in
            let bottom = assoc (x, y+1) board_list in
            is_valid_ref := ((top <> col) && (left<> col) && (bottom <> col));
            t_cols_ref := t;
          )
          else if (y=0) then (
            let left = assoc (x-1, y) board_list in
            let right = assoc (x+1, y) board_list in
            let bottom = assoc (x, y+1) board_list in
            is_valid_ref := ((left <> col) && (right <> col) && (bottom <> col));
            t_cols_ref := t;
          )
          else if (y=highest_i) then (
            let top = assoc (x, y-1) board_list in
            let right = assoc (x+1, y) board_list in
            let left = assoc (x-1, y) board_list in
            is_valid_ref := ((top <> col) && (right <> col) && (left <> col));
            t_cols_ref := t;
          )
          else (
            let top = assoc (x, y-1) board_list in
            let right = assoc (x+1, y) board_list in
            let left = assoc (x-1, y) board_list in
            let bottom = assoc (x, y+1) board_list in
            is_valid_ref := ((top <> col) && (right <> col) && (left <> col)
                             && (bottom <> col));
            t_cols_ref := t;
          )
        )
      end
  )
  done; !is_valid_ref

(* [check_corners t brd] is true if at least one of the corners/vertices of [t]
   is touching another corner or vertice of a tile of the same color that is
   already on [brd]. false otherwise. *)
let rec check_corners c_on_b t brd acc =
  let brd_lst = to_list brd in
  match c_on_b with
  | [] -> acc
  | ((x,y),corner_col)::tl -> begin
      let brd_color = assoc (x,y) brd_lst in
      if (brd_color = White) then check_corners tl t brd (0::acc)
      else if (brd_color = corner_col) then check_corners tl t brd (1::acc)
      else check_corners tl t brd (2::acc)
  end

(* [outside_board_invalid invalid_coord_list] looks at the coordinates that lay
    outside the dimensions of the board.
    returns: true if all the elements in [invalid_coord_list]
    are of white color or [invalid_coord_list] = []. false otherwise. *)
let outside_board_is_invalid invalid_coord_list =
  let invalid_brd_coord = ref invalid_coord_list in
  let invalid_coord_has_no_color = ref true in
  while (!invalid_brd_coord <> [] && !invalid_coord_has_no_color) do (
    match !invalid_brd_coord with
    |[] -> ()
    |((x,y), col)::t -> begin
      if col = White then invalid_coord_has_no_color:= true
      else invalid_coord_has_no_color := false;
      invalid_brd_coord := t;
    end
  ) done; !invalid_coord_has_no_color

(* [is_on_free_space t_cols_only is_free b] is true if the tile is placed on spaces
    where the board is free. In other words, there have not already been tiles placed on
    those coordinates. false otherwise.  *)
let is_on_free_space t_cols_only is_free b =
  let t_cols_ref = ref t_cols_only in
  while (!t_cols_ref <> [] && !is_free) do (
    match !t_cols_ref with
    | [] -> ()
    | ((x,y),col)::t -> begin
        let board_list = b |> to_list in
        let b_col = assoc (x,y) board_list in
        if (b_col = White) then is_free := true else is_free := false;
        t_cols_ref := t;
    end
  ) done; !is_free

(* [valid_first_move p tile_colors invalid_coords brd highest_i] is true if the
   desired location that [p] wants to place a tile on [brd] is legal. false otherwise.
   this function specifically checks the validity of a move for the first move of a
   player because the first move of a player must satisfy:
   1) Player 1 must play a piece in the top left hand corner.
   2) Player 2 must play a piece in the bottom right corner.*)
let valid_first_move p tile_colors invalid_coords brd highest_i =
  let out_of_bounds = outside_board_is_invalid invalid_coords in
  if (p.player_name = "Player 1") then
    (mem_assoc (0,0) tile_colors && assoc (0,0) tile_colors = Yellow)
    && out_of_bounds
  else (mem_assoc (highest_i, highest_i) tile_colors &&
        assoc (highest_i,highest_i) tile_colors = Blue) && out_of_bounds

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
let is_valid_move p st tl dot highest_i t_cols b_cols invalid_coords =
  let not_out_of_bounds = outside_board_is_invalid invalid_coords in
  let t_on_b = convert_to_board_coors dot tl.grid in
  let c_on_b = corners_grid_to_board_coords dot tl t_on_b highest_i in
  let t_cols_only = extract_t_cols_only t_cols in
  let tile_colors_ref = ref t_cols_only in
  let valid_sides_ref = ref true in
  let is_free_ref = ref true in
  let valid_corners = mem (1) ([]|> check_corners c_on_b tl st.board) in
  let valid_sides = check_sides tile_colors_ref st.board highest_i valid_sides_ref in
  let is_free_space = is_on_free_space t_cols_only is_free_ref st.board in
  not_out_of_bounds && valid_corners && valid_sides && is_free_space

(*[get_tile_from_tile_id st id] is the tile whose tile_id is [id].*)
let get_tile_from_tile_id st id =
  find (fun elt -> id = elt.name) (st.curr_player).remaining_tiles

(* [invalid_board_coords t_w_b_coords max_idx] extracts the coordinates from
    [t_w_b_coords] that are out of the dimensions of the board. *)
let invalid_board_coords t_w_b_coords max_idx =
  filter (fun ((x,y),col) -> x < 0 || x > max_idx || y < 0 || y > max_idx)
    t_w_b_coords

(* [place_tile_helper p st t t_grid_cols t_cols invalid_coords b max_i] places
    a tile on the board if the tile [t] to be placed is to be placed in a valid
    position.
    it removes the placed tile from [p's] remaining tiles and then switches
    the player. *)
let place_tile_helper p st t t_grid_cols t_cols invalid_coords b max_i =
    place_tile_on_brd t_cols st.board; player_place_tile p t;
    p.status <- Play;
    if p.player_name = "Player 1" then (
      if ((nth st.players 1).status = Stop) then st.canvas1 <- empty_grid
      else (st.curr_player <- nth st.players 1; st.canvas1 <- empty_grid;))
    else (
      if ((nth st.players 0).status = Stop) then st.canvas2 <- empty_grid
      else (st.curr_player <- nth st.players 0; st.canvas2 <- empty_grid;)
    )

(* [place_tile' st p t_id (x,y) is_p1] places [t] onto the board if the choice
   of placement (x,y) is a legal move. If it is, it removes [t] from [p's]
  remaining_tiles. else, it does not do anything. *)
let place_tile' st p t_id (x,y) is_p1 =
  let t = get_tile_from_tile_id st t_id in
  let max_i = (st.board |> brd_size) - 1 in
  let dot = (x,y) |> get_center_cell st in
  let coordinates = dot |> get_selection_space_coords in
  let colors_on_board = [] |> get_board_colors st.board coordinates in
  let t_to_b = t.grid |> convert_to_board_coors dot in
  let invalid_coords = max_i |> invalid_board_coords t_to_b in
  let colors_of_tile = [] |> get_tile_colors t_to_b colors_on_board in
  let tile_only = extract_t_cols_only colors_of_tile in
  if is_p1 then t.grid <- st.canvas1 else t.grid <- st.canvas2;
  if (p.status = Start) then (
    if (valid_first_move p colors_of_tile invalid_coords st.board max_i) then
    place_tile_helper p st t colors_of_tile tile_only invalid_coords st.board max_i)
  else (
    if (is_valid_move p st t dot max_i colors_of_tile colors_on_board invalid_coords)
    then place_tile_helper p st t colors_of_tile tile_only invalid_coords st.board max_i
  )

(* [update_state c st] updates [st] based on what [c] is. *)
let update_state c st =
  let curr_p = st.curr_player in
  let p1_curr_player = curr_p.player_name = "Player 1" in
  match c with
  | PLACE ((x,y), t_id) -> begin
      place_tile' st curr_p t_id (x,y) p1_curr_player;
      if curr_p.remaining_tiles = [] then curr_p.status <- Stop;
      if (List.nth st.players 0).status = Stop && (List.nth st.players 1).status = Stop then
        st.game_over <- true; 
      st
    end
   | FLIPX t_id -> begin
     let t = get_tile_from_tile_id st t_id in
     if p1_curr_player then st.canvas1 <- (flip_tile t X).grid
     else st.canvas2 <- (flip_tile t X).grid; st
   end
   | FLIPY t_id -> begin
     let t = get_tile_from_tile_id st t_id in
     if p1_curr_player then st.canvas1 <- (flip_tile t Y).grid
     else st.canvas2 <- (flip_tile t Y).grid; st
   end
   | TURN t_id -> begin
     let t = get_tile_from_tile_id st t_id in
     if p1_curr_player then st.canvas1 <- (turn_tile t).grid
     else st.canvas2 <- (turn_tile t).grid; st
   end
   | FORFEIT -> failwith "Won't happen"

let do_command c st =
  let p1 = nth st.players 0 in
  let p2 = nth st.players 1 in
  let curr_p = st.curr_player in
  match c with
  | FLIPX t_id -> update_state c st
  | FLIPY t_id -> update_state c st
  | TURN t_id -> update_state c st
  | PLACE ((x,y),t_id) -> update_state c st
  | FORFEIT -> begin
      if curr_p.player_name = "Player 1" then
        (p1.status <- Stop; st.curr_player <- nth st.players 1)
      else (p2.status <- Stop; st.curr_player <- nth st.players 0;);
      if p1.status = Stop && p2.status = Stop then st.game_over <- true; st
    end

let print_winner st =
  if (nth st.players 0).score = (nth st.players 1).score
    then "      It's a Tie!      "
  else if (nth st.players 0).score > (nth st.players 1).score
    then "The Winner is Player 1!"
  else "The Winner is Player 2!"
