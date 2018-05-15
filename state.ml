open Player
open Board
open Tile
open Command


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
  let player_1 = Player.init_player "Player 1" Yellow in
  let player_2 = Player.init_player "Player 2" Blue in
  {
  board = (init_board s);
  players = [player_1; player_2];
  canvas1 = empty_grid;
  canvas2 = empty_grid;
  curr_player = player_1;
  game_over = false
}

  let get_center_cell st (x,y) =
  let brd_coord = (x - 400) / 40 , -(((y - 175) / 40) - 9) in
    let index = get_index brd_coord (brd_size st.board) in
    fst (Array.get st.board index)

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
      if (List.mem_assoc (x,y) lst_of_board) then (
        let colors = List.assoc (x,y) lst_of_board in
        get_board_colors brd t (((x,y),colors)::acc)
      ) else get_board_colors brd t acc
    end

(* [tile_coords_to_board_coords (x,y) t_grid acc] converts the coordinates in [t_grid]
to their corresponding board coordinates based on the center cell (x,y) *)
let  tile_coords_to_board_coords (x,y) t_grid =
  [(x-1,y-1),(List.assoc (-1,1) t_grid); ((x,y-1),(List.assoc (0,1) t_grid));
   (x+1,y-1),(List.assoc (1,1) t_grid);  (x-1,y),(List.assoc (-1,0) t_grid);
   (x,y),(List.assoc (0,0) t_grid);      (x+1,y),(List.assoc (1,0) t_grid);
   (x-1,y+1),(List.assoc (-1,-1) t_grid);(x,y+1),(List.assoc (0,-1) t_grid);
   (x+1,y+1),(List.assoc (1,-1)) t_grid]

(* [tile_coords_to_board_coords (x,y) t_grid acc] converts the coordinates in [t_grid]
   to their corresponding board coordinates based on the center cell (x,y) *)
let  tile_coords_to_board_coords2 (x,y) c_grid =
  [(x-2,y-2),(List.assoc (-2,2) c_grid); ((x-1,y-2),(List.assoc (-1,2) c_grid));
   (x,y-2),(List.assoc (0,2) c_grid); ((x+1,y-2),(List.assoc (1,2) c_grid));
   (x+2,y-2),(List.assoc (2,2) c_grid);
   (x-2,y-1),(List.assoc (-2,1) c_grid);  (x-1,y-1),(List.assoc (-1,1) c_grid);
   (x,y-1),(List.assoc (0,1) c_grid); (x+1,y-1),(List.assoc (1,1) c_grid);
   (x+2,y-1),(List.assoc (2,1) c_grid);
   (x-2,y),(List.assoc (-2,0) c_grid);(x-1,y),(List.assoc (-1,0) c_grid);
   (x,y),(List.assoc (0,0)) c_grid;(x+1,y),(List.assoc (1,0) c_grid);
   (x+2,y),(List.assoc (2,0) c_grid);
   (x-2,y+1),(List.assoc (-2,-1) c_grid);(x-1,y+1),(List.assoc (-1,-1) c_grid);
   (x,y+1),(List.assoc (0,-1)) c_grid;(x+1,y+1),(List.assoc (1,-1) c_grid);
   (x+2,y+1),(List.assoc (2,-1) c_grid);
   (x-2,y+2),(List.assoc (-2,-2) c_grid);(x-2,y+2),(List.assoc (-1,-2) c_grid);
   (x,y+2),(List.assoc (0,-2)) c_grid;(x+1,y+2),(List.assoc (1,-2) c_grid);
   (x+2,y+2),(List.assoc (2,-2) c_grid)]

(* [corners_grid_to_board_coords (x,y) c_grid t_to_b max_i] converts the
   coordinates in [c_grid]to their corresponding board coordinates based on
   the center cell (x,y). in other words, it returns a list of the corners'
   coordinates and colors. then, filters out any of the coordinates that are
   1) out of bounds of the dimensions of the board
   2) part of the tile itself
   3) colored white*)
let corners_grid_to_board_coords (x,y) t t_to_b max_i =
  let c_grid = grid_of_corners t in
  let new_c_grid =
  [(x-2,y-2),(List.assoc (-2,2) c_grid); ((x-1,y-2),(List.assoc (-1,2) c_grid));
   (x,y-2),(List.assoc (0,2) c_grid); ((x+1,y-2),(List.assoc (1,2) c_grid));
   (x+2,y-2),(List.assoc (2,2) c_grid);
   (x-2,y-1),(List.assoc (-2,1) c_grid);  (x-1,y-1),(List.assoc (-1,1) c_grid);
   (x,y-1),(List.assoc (0,1) c_grid); (x+1,y-1),(List.assoc (1,1) c_grid);
   (x+2,y-1),(List.assoc (2,1) c_grid);
   (x-2,y),(List.assoc (-2,0) c_grid);(x-1,y),(List.assoc (-1,0) c_grid);
   (x,y),(List.assoc (0,0)) c_grid;(x+1,y),(List.assoc (1,0) c_grid);
   (x+2,y),(List.assoc (2,0) c_grid);
   (x-2,y+1),(List.assoc (-2,-1) c_grid);(x-1,y+1),(List.assoc (-1,-1) c_grid);
   (x,y+1),(List.assoc (0,-1)) c_grid;(x+1,y+1),(List.assoc (1,-1) c_grid);
   (x+2,y+1),(List.assoc (2,-1) c_grid);
   (x-2,y+2),(List.assoc (-2,-2) c_grid);(x-1,y+2),(List.assoc (-1,-2) c_grid);
   (x,y+2),(List.assoc (0,-2)) c_grid;(x+1,y+2),(List.assoc (1,-2) c_grid);
   (x+2,y+2),(List.assoc (2,-2) c_grid)] in
  let remove_whites = List.filter (fun ((x,y),c) -> (c = White) = false)
      new_c_grid in
  let remove_out_of_bounds = List.filter (fun ((x,y),c) ->
      x >= 0 && x <= max_i && y >= 0 && y <= max_i)
      remove_whites in
  List.filter (fun t_coor -> (List.mem t_coor t_to_b) = false) remove_out_of_bounds

(* [tl_b_coords b_cols acc] is a list of the coordinates mapped to their colors
    when the tile coordinates are mapped to board cooordinates. In other words, color
    does not change but the tile coordinates are in terms of the board. *)
let rec get_tile_colors tl_b_coords b_cols acc =
  match b_cols with
  | [] -> acc
  | (coord,col)::t -> begin
    (* if (List.mem_assoc h tl_b_coords) then ( *)
      let colors = List.assoc coord tl_b_coords in
      get_tile_colors tl_b_coords t ((coord,colors)::acc)
       (* else get_tile_colors tl_b_coords t acc *)
    end

(* [extract_t_cols_only t_grid_cols] is the list representing the tile itself,
   not the entire canvas that includes white spaces. *)
let extract_t_cols_only t_grid_cols =
  List.filter (fun ((x,y),col) -> col != White) t_grid_cols

(* [check_sides t_cols_ref brd_array highest_i is_valid_ref]
    is true if the tile to be placed/played is to be placed in a space that does not result in
    any of the sides of tile touching the sides of other tiles on the board of the same color. *)
let check_sides t_cols_ref brd_array highest_i is_valid_ref =
  while ((!t_cols_ref <> []) && !is_valid_ref) do (
    match !t_cols_ref with
    | [] -> ()
    | ((x,y),col)::t -> begin
        let board_list = Array.to_list brd_array in
        if (x=0 && y=0) then (
          Printf.printf("hello1");
          let right = List.assoc (x+1,y) board_list in
          let bottom = List.assoc (x, y+1) board_list in
          is_valid_ref := ((right <> col) && (bottom <> col));
          t_cols_ref := t;
        )(*check right and bottom) *)
        else if (x=0 && y=highest_i) then (
          Printf.printf("hello2");
          let top = List.assoc (x, y-1) board_list in
          let right = List.assoc (x+1, y) board_list in
          is_valid_ref := ((top <> col) && (right <> col));
          t_cols_ref := t;
        )(*check top and right*)
        else if (x=highest_i && y=0) then (
          Printf.printf("hello3");
          let left = List.assoc (x-1,y) board_list in
          let bottom = List.assoc (x, y+1) board_list in
          is_valid_ref := ((left <> col) && (bottom <> col));
          t_cols_ref := t;
        ) (*check left and bottom*)
        else if (x=highest_i && y=highest_i) then (
          Printf.printf("hello4");
          let top = List.assoc (x, y-1) board_list in
          let left = List.assoc (x-1,y) board_list in
          is_valid_ref := ((left <> col) && (top <> col));
          t_cols_ref := t;
        ) (*check top and left*)
        else (
          if (x=0) then (
            Printf.printf("hello5");
            let top = List.assoc (x, y-1) board_list in
            let right = List.assoc (x+1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            is_valid_ref := ((top <> col) && (right <> col) && (bottom <> col));
            t_cols_ref := t;
          ) (*don't check the left*)
          else if (x=highest_i) then (
            Printf.printf("hello6");
            let top = List.assoc (x, y-1) board_list in
            let left = List.assoc (x-1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            is_valid_ref := ((top <> col) && (left<> col) && (bottom <> col));
            t_cols_ref := t;
          ) (*dont check right*)
          else if (y=0) then (
            Printf.printf("hello7");
            let left = List.assoc (x-1, y) board_list in
            let right = List.assoc (x+1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            is_valid_ref := ((left <> col) && (right <> col) && (bottom <> col));
            t_cols_ref := t;
          ) (*dont check top*)
          else if (y=highest_i) then (
            Printf.printf("hello8");
            let top = List.assoc (x, y-1) board_list in
            let right = List.assoc (x+1, y) board_list in
            let left = List.assoc (x-1, y) board_list in
            is_valid_ref := ((top <> col) && (right <> col) && (left <> col));
            t_cols_ref := t;
          ) (*don't check bottom*)
          else (
            Printf.printf "hello9";
            let top = List.assoc (x, y-1) board_list in
            let right = List.assoc (x+1, y) board_list in
            let left = List.assoc (x-1, y) board_list in
            let bottom = List.assoc (x, y+1) board_list in
            is_valid_ref := ((top <> col) && (right <> col) && (left <> col)
                             && (bottom <> col));
            t_cols_ref := t;
          ) (*check everything*)
        )
      end
  )
  done; !is_valid_ref

(* let check_corners c_on_b t brd =
let brd_lst = Array.to_list brd in
let valid_corners_ref = ref c_on_b in
let is_valid_ref = ref true in

while (!valid_corners_ref <> [] && !is_valid_ref) do (
  match !valid_corners_ref with
  |[] -> ()
  |((x,y),c)::tl -> begin
      let brd_color = List.assoc (x,y) brd_lst in
      if (brd_color = t.col) || (brd_color = White) then is_valid_ref := true
      else is_valid_ref := false;
      valid_corners_ref := tl;
    end
)
done; !is_valid_ref
 *)

(* [check_corners t brd] is true if at least one of the corners/vertices of [t] is
    touching another corner or vertice of a tile of the same color that is already on
    [brd]. false otherwise. *)
let rec check_corners c_on_b t brd acc =
  let brd_lst = Array.to_list brd in
  match c_on_b with
  | [] -> acc
  | ((x,y),corner_col)::tl -> begin
      let brd_color = List.assoc (x,y) brd_lst in
      (* let is_white = 0 in
      let is_col = 1 in
      let is_other_col = 2 in  *)
      if (brd_color = White) then check_corners tl t brd (0::acc)
      else if (brd_color = corner_col) then check_corners tl t brd (1::acc)
      else check_corners tl t brd (2::acc)
  end

(* [check_corners t brd] is true if at least one of the corners/vertices of [t] is
    touching another corner or vertice of a tile of the same color that is already on
    [brd]. false otherwise.
let check_corners c_on_b t brd =
  let brd_lst = Array.to_list brd in
  let valid_corners_ref = ref c_on_b in
  let is_valid_ref = ref true in

  while (!valid_corners_ref <> [] && !is_valid_ref) do (
    match !valid_corners_ref with
    |[] -> ()
    |((x,y),c)::tl -> begin
        let brd_color = List.assoc (x,y) brd_lst in
        if (brd_color = t.col) || (brd_color = White) then is_valid_ref := true
        else is_valid_ref := false;
        valid_corners_ref := tl;
      end
  )
   done; !is_valid_ref*)

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
  )
  done; !invalid_coord_has_no_color

(* [is_on_free_space t_cols_only is_free b] is true if the tile is placed on spaces
    where the board is free. In other words, there have not already been tiles placed on
    those coordinates. false otherwise.  *)
let is_on_free_space t_cols_only is_free b =
  let t_cols_ref = ref t_cols_only in
  while (!t_cols_ref <> [] && !is_free) do (
    match !t_cols_ref with
    | [] -> ()
    | ((x,y),col)::t -> begin
        let board_list = Array.to_list b in
        let b_col = List.assoc (x,y) board_list in
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
    (List.mem_assoc (0,0) tile_colors && List.assoc (0,0) tile_colors = Yellow)
    && out_of_bounds
  else (List.mem_assoc (highest_i, highest_i) tile_colors &&
        List.assoc (highest_i,highest_i) tile_colors = Blue) && out_of_bounds

(* [is_valid_move p st tl highest_i t_cols b_cols invalid_coords] is true if the
   desired location that [p] wants to place a tile on [st.board] is legal.
   false otherwise.
   this function specifically checks the validity of a move for any move of a
   player after the first move. a move is valid if:
   1) A corner of [tl] is touching the corner of another tile of the same color
      that is already placed on the board.
   2) None of the sides (or edges) of [tl] is touching a side (or edge) or another
      tile of the same color that is already placed on the board.*)
 (*may not need specification because in mli file*)
let is_valid_move p st tl dot highest_i t_cols b_cols invalid_coords =
  let not_out_of_bounds = outside_board_is_invalid invalid_coords in
  let t_on_b = tile_coords_to_board_coords dot tl.grid in
  (* let c_grid = tl |> grid_of_corners in
  let c_grid_on_b = tile_coords_to_board_coords dot c_grid in *)
  let c_on_b = corners_grid_to_board_coords dot tl t_on_b highest_i in
  let t_cols_only = extract_t_cols_only t_cols in
  let tile_colors_ref = ref t_cols_only in
  let valid_sides_ref = ref true in
  let is_free_ref = ref true in
  let valid_corners = List.mem (1) ([]|> check_corners c_on_b tl st.board) in
  let valid_sides = check_sides tile_colors_ref st.board highest_i valid_sides_ref in
  let is_free_space = is_on_free_space t_cols_only is_free_ref st.board in
  not_out_of_bounds && valid_corners && valid_sides && is_free_space

(*[get_tile_from_tile_id st id] is the tile whose tile_id is [id].*)
let get_tile_from_tile_id st id =
  List.find (fun elt -> id = elt.name) (st.curr_player).remaining_tiles

(* [invalid_board_coords t_w_b_coords max_idx] extracts the coordinates from
    [t_w_b_coords] that are out of the dimensions of the board. *)
let invalid_board_coords t_w_b_coords max_idx =
  List.filter (fun ((x,y),col) -> x < 0 || x > max_idx || y < 0 || y > max_idx)
  t_w_b_coords

(* [place_tile' st p t_id (x,y) is_p1] places [t] onto the board if the choice
   of placement (x,y) is a legal move. If it is, it removes [t] from [p's]
  remaining_tiles. else, it does not do anything. *)
let place_tile' st p t_id (x,y) is_p1 =
  let t = get_tile_from_tile_id st t_id in
  let max_i = (st.board |> brd_size) - 1 in
  let dot = (x,y) |> get_center_cell st in
  let coordinates = dot |> get_selection_space_coords in
  let colors_on_board = [] |> get_board_colors st.board coordinates in
  let t_to_b = t.grid |> tile_coords_to_board_coords dot in
  let invalid_coords = max_i |> invalid_board_coords t_to_b in
  let colors_of_tile = [] |> get_tile_colors t_to_b colors_on_board in
  if is_p1 then t.grid <- st.canvas1 else t.grid <- st.canvas2;
  if (p.status = Start) then (
    if (valid_first_move p colors_of_tile invalid_coords st.board max_i) then (
      place_tile_on_brd colors_of_tile st.board; player_place_tile p t;
      p.status <- Play;
      if p.player_name = "Player 1" then (
        if (List.nth st.players 1).status = Stop then st.canvas1 <- empty_grid
        else (st.curr_player <- List.nth st.players 1; st.canvas1 <- empty_grid;)
      else (
      if (List.nth st.players 0).status = Stop then st.canvas2 <- empty_grid
      else (st.curr_player <- List.nth st.players 0; st.canvas2 <- empty_grid;)
    )
  ) else (
    if (is_valid_move p st t dot max_i colors_of_tile colors_on_board invalid_coords)
    then (place_tile_on_brd colors_of_tile st.board;
          player_place_tile p t;
          if p.player_name = "Player 1" then (
            if (List.nth st.players 1).status = Stop then st.canvas1 <- empty_grid
            else (st.curr_player <- List.nth st.players 1; st.canvas1 <- empty_grid;)
          else (
            if (List.nth st.players 0).status = Stop then st.canvas2 <- empty_grid
            else (st.curr_player <- List.nth st.players 0; st.canvas2 <- empty_grid;)
          )
  )

(* [update_state c st] updates [st] based on what [c] is. *)
let update_state c st =
  let curr_p = st.curr_player in
  let p1_curr_player = curr_p.player_name = "Player 1" in
  match c with
  | PLACE ((x,y), t_id) -> begin
      place_tile' st curr_p t_id (x,y) p1_curr_player;
      (*only happens if a move was valid and placed, right*)
      (* if curr_p.status = Start then curr_p.status <- Play; *)
      (* if curr_p.remaining_tiles = [] then curr_p.status <- Stop; *)
      (* if curr_p.player_name = "Player 1" then
        (st.curr_player <- List.nth st.players 1; st.canvas1 <- empty_grid;)
      else  (st.curr_player <- List.nth st.players 0; st.canvas2 <- empty_grid;); *)
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
   | FORFEIT -> failwith "Impossible"

let do_command c st =
  let p1 = List.nth st.players 0 in
  let p2 = List.nth st.players 1 in
  let curr_p = st.curr_player in
  match c with
  | FLIPX t_id -> update_state c st
  | FLIPY t_id -> update_state c st
  | TURN t_id -> update_state c st
  | PLACE ((x,y),t_id) -> update_state c st
  | FORFEIT -> begin
      if curr_p.player_name = "Player 1" then (p1.status <- Stop; st.curr_player <- List.nth st.players 1)
      else (p2.status <- Stop; st.curr_player <- List.nth st.players 0;);
      if p1.status = Stop && p2.status = Stop then st.game_over <- true; st
    end

let print_winner st =
  if (List.nth st.players 0).score = (List.nth st.players 1).score then "It's a Tie!!!"
      else if (List.nth st.players 0).score > (List.nth st.players 1).score then "The Winner is Player 1!"
    else "The Winner is Player 2!"

(*[col_to_name col] is a string representation of [col].*)
let col_to_name col =
  match col with
  |Blue -> "B"
  |Yellow -> "Y"
  |White -> "W"


let rec print_state brd =
let brd_lst = Array.to_list brd in
let str =
 match brd_lst with
 |[] -> ""
 |((x,_),col)::t -> if x <> 7 then col_to_name col ^ " " else col_to_name col ^ "\n"
in print_string str
