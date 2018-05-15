open OUnit2
open State
open Tile
open Player
open List

let rec compare_lsts lst1 lst2 =
  match lst1 with
  | [] -> true
  | h::t -> mem h lst2 && compare_lsts t lst2

let t1 = init_tile One Blue
let grid_t1 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
               ((-1,0),White);  ((0,0),Blue);      ((1,0),White);
               ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
let corners_t = [(-1,1);(1,1);(-1,-1);(1,-1)]

let t2 = init_tile One Yellow
let t3 = init_tile One Yellow

(* L Tile Tests *)
let t1_L = init_tile L Blue
let grid_of_t1_L = [((-1,1),Blue);  ((0,1),White);  ((1,1),White);
                    ((-1,0),Blue);  ((0,0),White);      ((1,0),White);
                    ((-1,-1),Blue); ((0,-1),Blue); ((1,-1),Blue)]
let corners_of_t1_L = [(-2,2);(0,2);(-2,-2);(2,-2);(2,0)]
let flip_X_corners = [(-2,2);(2,2);(0,-2);(-2,-2);(2,0)]
let turn_flip_X = [(2,-2);(2,2);(0,-2);(-2,2);(-2,0)]
let turn_2 = [(0,2);(2,2);(2,-2);(-2,-2);(-2,0)]
let flip_Y_corners = [(0,2);(2,2);(2,-2);(-2,-2);(-2,0)]

(* X Tile Tests *)
let t1_x = init_tile X Blue
let grid_of_X = [((-1,1),White);  ((0,1),Blue);  ((1,1),White);
                 ((-1,0),Blue);  ((0,0),Blue);      ((1,0),Blue);
                 ((-1,-1),White); ((0,-1),Blue); ((1,-1),White)]
let x_Corners = [(-1,2);(1,2);(2,1);(2,-1);(-2,1);(-2,-1);(-1,-2);(1,-2)]

(* Line Tile Tests *)
let t1_line = init_tile X Yellow
let grid_of_line = [((-1,1),White);  ((0,1),Yellow);  ((1,1),White);
                    ((-1,0),White);  ((0,0),Yellow);      ((1,0),White);
                    ((-1,-1),White); ((0,-1),Yellow); ((1,-1),White)]
let line_corns = [(-1,2);(1,2);(1,-2);(-1,-2)]
let turn1_corns = [(-2,1);(2,1);(2,-1);(-2,-1)]

(* Tee Tile Tests *)
let t1_tee = init_tile Tee Yellow
let grid_of_tee = [((-1,1),Yellow);      ((0,1),Yellow);  ((1,1),Yellow);
                   ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                   ((-1,-1),White); ((0,-1),Yellow); ((1,-1),White)]
let tee_corns = [(-2,2);(2,2);(-1,-2);(1,-2);(2,0);(-2,0)]
let turn1_tee_corns = [(-2,1);(-2,-1);(0,2);(0,-2);(2,2);(2,-2)]
let flip_X_tee_corns = [(-1,2);(1,2);(2,0);(2,-2);(-2,-2);(-2,0)]
let turn3_tee_corns = [(2,1);(2,-1);(0,-2);(-2,-2);(-2,-2);(0,2)]

(* Z Tile Tests *)
let t1_z = init_tile Z Yellow
let grid_of_z = [((-1,1),Yellow);  ((0,1),Yellow);  ((1,1),White);
                 ((-1,0),White);  ((0,0),Yellow);      ((1,0),White);
                 ((-1,-1),White); ((0,-1),Yellow); ((1,-1),Yellow)]
let z_corns = [(-2,2);(-2,0);(1,2);(-1,-2);(2,0);(2,-2)]
let flip_Y_z_corns = [(-1,2);(1,-2);(2,0);(2,2);(-2,-2);(-2,0)]
let flipturn_z_corns = [(-2,2);(0,2);(2,1);(2,-2);(0,-2);(-2,-1)]

(* Tree Tile Tests *)
let t_tree = init_tile Tree Blue
let tree_corns = [(-2,2);(-2,0);(1,2);(2,1);(2,-1);(1,-2);(-1,-2)]
let turn1_tree = [(0,2);(2,2);(2,-1);(1,-2);(-1,-2);(-2,-1);(-2,1)]
let turnflip_tree = [(-1,2);(1,2);(2,-2);(0,-2);(-2,-1);(-2,1);(2,1)]

(*****************************State Tests****************************)

(*initial states*)
let init = init_state 16
let init2 = init_state 16
let init2' = {init2 with canvas1 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                    ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                                    ((-1,-1),White); ((0,-1),White); ((1,-1),White)];}
let init3 = init_state 16
let init3' = {init3 with canvas1 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                    ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                                    ((-1,-1),White); ((0,-1),White); ((1,-1),White)];}

let init3 = init_state 16
let init3' = {init3 with canvas1 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                    ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                                    ((-1,-1),White); ((0,-1),White); ((1,-1),White)];}

let player_1 = init_player "Player 1" Yellow
let player_2 = init_player "Player 2" Blue
let t1_init = init_tile One Yellow
let t1_init2 = init_tile One Yellow
let t1_init3 = init_tile One Yellow
let t1_init4 = init_tile Tee Yellow
let t1_init5 = init_tile Cowgirl Yellow
let p1_m2 = init_tile Cowgirl Yellow
let p2_m2 = init_tile Cowgirl Blue
let t1_init6 = init_tile One Yellow
let t1_init7 = init_tile One Yellow
let t1_init8 = init_tile One Yellow
let t2_init = init_tile One Blue
let t2_init6 = init_tile One Blue
let t2_init7 = init_tile One Blue
let t2_init8 = init_tile One Blue
let p1_m2_8 = init_tile Cowgirl Yellow
let p2_m2_8 = init_tile Cowgirl Blue
let t1' = init_tile Tee Yellow

(*st2 - p1 first move*)
let st2 = do_command (PLACE ((404,570),t1_init.name)) init2'
let st2_invalid_move = do_command (PLACE ((780,176),t1_init2.name)) init3'

(*st3 - p2 first move*)
let init4 = 16 |> init_state |> do_command (PLACE ((404,570),t1_init.name))
let st3 =  init4 |> do_command (PLACE ((780,176),t2_init.name))

(*st4 - turn canvas*)
let init4 = 16 |> init_state
let init4' = {init4 with canvas1 = t1_init4.grid} |> do_command (TURN t1_init4.name)

(*st5 - flip canvas*)
let init5 = 16 |> init_state
let init5' = {init5 with canvas1 = t1_init5.grid} |> do_command (FLIPX t1_init5.name)

(*st6- p1 second move = play mode*)
let init6 = 16 |> init_state
let init6' = {init6 with canvas1 = t1_init6.grid} |> do_command (PLACE ((404,570),t1_init.name))
let init6'' = {init6' with canvas2 = t2_init6.grid} |> do_command (PLACE ((780,176),t2_init6.name))
let init6''' = {init6'' with canvas1 = p1_m2.grid} |> do_command (PLACE ((426,546),p1_m2.name))

(*st7- p2 second move = invalid*)
let init7 = 16 |> init_state
let init7' = {init7 with canvas1 = t1_init7.grid} |> do_command (PLACE ((404,570),t1_init7.name))
let init7'' = {init7' with canvas2 = t2_init7.grid} |> do_command (PLACE ((780,176),t2_init7.name))
let init7_3 = {init7'' with canvas1 = p1_m2.grid} |> do_command (PLACE ((426,546),p1_m2.name))
let init7_p2 = {init7_3 with canvas2 = p2_m2.grid} |> do_command (PLACE ((600,546),p2_m2.name))

(*st8- p2 second move = valid*)
let init8 = 16 |> init_state
let init8' = {init8 with canvas1 = t1_init8.grid} |> do_command (PLACE ((404,570),t1_init8.name))
let init8'' = {init8' with canvas2 = t2_init8.grid} |> do_command (PLACE ((780,176),t2_init8.name))
let init8_3 = {init8'' with canvas1 = p1_m2.grid} |> do_command (PLACE ((426,546),p1_m2.name))
let init8_p2 = {init8_3 with canvas2 = p2_m2.grid} |> do_command (PLACE ((746,226),p2_m2.name))

(*st9 - both = forfeit*)
let init9 = 16 |> init_state
let init9' = {init9 with canvas1 = t1_init6.grid} |> do_command (PLACE ((404,570),t1_init.name))
let init9'' = {init9' with canvas2 = t2_init6.grid} |> do_command (PLACE ((780,176),t2_init6.name))
let init9_p1_f = (init9'' |> do_command FORFEIT) |> do_command FORFEIT

(*updating players*)
let p1_invalid_move1 = {player_name = "Player 1";
                        col = Yellow;
                        score = 0;
                        status = Start;
                        remaining_tiles = [init_tile One Yellow; init_tile Tee Yellow;
                                           init_tile L Yellow; init_tile X Yellow;
                                           init_tile Z Yellow; init_tile Tree Yellow;
                                           init_tile Line Yellow; init_tile Two Yellow;
                                           init_tile Four Yellow; init_tile Boot Yellow;
                                           init_tile Kink Yellow; init_tile C Yellow;
                                           init_tile Cowgirl Yellow; init_tile Couch Yellow;
                                           init_tile Recliner Yellow;init_tile Stairmaster Yellow];}

let p1_turn = {player_name = "Player 1";
                        col = Yellow;
                        score = 0;
                        status = Start;
                        remaining_tiles = [init_tile One Yellow; t1_init4;
                                           init_tile L Yellow; init_tile X Yellow;
                                           init_tile Z Yellow; init_tile Tree Yellow;
                                           init_tile Line Yellow; init_tile Two Yellow;
                                           init_tile Four Yellow; init_tile Boot Yellow;
                                           init_tile Kink Yellow; init_tile C Yellow;
                                           init_tile Cowgirl Yellow; init_tile Couch Yellow;
                                           init_tile Recliner Yellow;init_tile Stairmaster Yellow];}

let p1_move_1 = {player_1 with score = 1; status = Play;
                               remaining_tiles = [init_tile Tee Yellow;
                                                  init_tile L Yellow; init_tile X Yellow;
                                                  init_tile Z Yellow; init_tile Tree Yellow;
                                                  init_tile Line Yellow; init_tile Two Yellow;
                                                  init_tile Four Yellow; init_tile Boot Yellow;
                                                  init_tile Kink Yellow; init_tile C Yellow;
                                                  init_tile Cowgirl Yellow; init_tile Couch Yellow;
                                                  init_tile Recliner Yellow;init_tile Stairmaster Yellow]}
let p2_move_1 = {player_2 with score = 1; status = Play;
                 remaining_tiles = [init_tile Tee Yellow;
                  init_tile L Yellow; init_tile X Yellow;
                  init_tile Z Yellow; init_tile Tree Yellow;
                  init_tile Line Yellow; init_tile Two Yellow;
                  init_tile Four Yellow; init_tile Boot Yellow;
                  init_tile Kink Yellow; init_tile C Yellow;
                  init_tile Cowgirl Yellow; init_tile Couch Yellow;
                  init_tile Recliner Yellow;init_tile Stairmaster Yellow]}


(********************************************************)
(*************************TILE ROTATION AND FLIP TESTS***************************)
let tests = [

  (* One Tile Tests  *)
  "tile_name"     >:: (fun _ -> assert_equal One (tile_name t1));
  "tile_col"      >:: (fun _ -> assert_equal Blue (col t1));
  "tile_grid"     >:: (fun _ -> assert_equal grid_t1 (grid t1));
  "tile_corners"  >:: (fun _ -> assert_equal corners_t (t1.corners));
  (*obviously true*)
  "tile_corners2" >:: (fun _ -> assert_equal true (compare_lsts t1.corners (flip_tile t1 X).corners));

  (* L Tile Tests *)
  "L_tile"        >:: (fun _ -> assert_equal L (tile_name t1_L));
  "flip L"        >:: (fun _ -> assert_equal true (compare_lsts flip_X_corners (flip_tile t1_L X).corners));
  "turn_from_flip">:: (fun _ -> assert_equal true
                          (compare_lsts ((turn_tile (flip_tile (init_tile L Blue) X)).corners)
                             turn_flip_X));
  "turn_2"        >:: (fun _ -> assert_equal true (compare_lsts turn_2
                                                     (turn_tile (turn_tile (flip_tile (init_tile L Blue) X))).corners));
  "turn_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts corners_of_t1_L
                                                     (turn_tile (turn_tile (turn_tile (flip_tile (init_tile L Blue) X)))).corners));
  "flip_Y"        >:: (fun _ -> assert_equal true (compare_lsts flip_Y_corners
                                                     (flip_tile (turn_tile (turn_tile (turn_tile (flip_tile (init_tile L Blue) X)))) Y).corners));

  (* X Tile Tests *)
  (* "X_tile"        >:: (fun _ -> assert_equal X (tile_name t1_x)); *)
  "flip X"         >:: (fun _ -> assert_equal true (compare_lsts x_Corners (flip_tile t1_x X).corners));
  "turn_frm_flip"  >:: (fun _ -> assert_equal true (compare_lsts x_Corners
                                                      (turn_tile (flip_tile (init_tile X Blue) X)).corners));
  "turn_2'"        >:: (fun _ -> assert_equal true (compare_lsts x_Corners
                                                      (turn_tile (turn_tile (flip_tile (init_tile X Blue) X))).corners));
  "turn_to_norm'"  >:: (fun _ -> assert_equal true (compare_lsts x_Corners
                                                      (turn_tile (turn_tile (turn_tile (flip_tile (init_tile X Blue) X)))).corners));
  "flip_Y'"        >:: (fun _ -> assert_equal true (compare_lsts x_Corners
                                                      (flip_tile (turn_tile (turn_tile (turn_tile (flip_tile (init_tile X Blue) X)))) Y).corners));

  (* Line Tile Tests *)
  "flip line"      >:: (fun _ -> assert_equal true (compare_lsts line_corns (flip_tile t1_line X).corners));
  "flip line norm" >:: (fun _ -> assert_equal true (compare_lsts line_corns
                                                      (flip_tile (flip_tile (init_tile Line Blue) X) X).corners));
  "flip over Y"    >:: (fun _ -> assert_equal true (compare_lsts line_corns
                                                      (flip_tile (init_tile Line Blue) Y).corners));
  "turn line"      >:: (fun _ -> assert_equal true
                           (compare_lsts turn1_corns (turn_tile (init_tile Line Blue)).corners));
  "turn_2_line"    >:: (fun _ -> assert_equal true (compare_lsts line_corns
                                                      (turn_tile (turn_tile (init_tile Line Blue))).corners));
  "turn_3_line"    >:: (fun _ -> assert_equal true (compare_lsts turn1_corns
                                                      (turn_tile (turn_tile (turn_tile (init_tile Line Blue)))).corners));

  (* Tee Tile Tests *)
  "turn_1_Tee"       >:: (fun _ -> assert_equal true (compare_lsts turn1_tee_corns (turn_tile t1_tee).corners));
  "turn_2_Tee"       >:: (fun _ -> assert_equal true (compare_lsts flip_X_tee_corns
                                                        (turn_tile (turn_tile (init_tile Tee Blue))).corners));
  "flip Y"           >:: (fun _ -> assert_equal true (compare_lsts flip_X_tee_corns
                                                        (flip_tile (turn_tile (turn_tile (init_tile Tee Blue))) Y).corners));
  "turn_from_flip">:: (fun _ -> assert_equal true (compare_lsts tee_corns
                                                     (flip_tile (flip_tile (turn_tile (turn_tile (init_tile Tee Blue))) Y) X).corners));
  "turn_again"    >:: (fun _ -> assert_equal true (compare_lsts turn3_tee_corns
                                                     (turn_tile (flip_tile (turn_tile (turn_tile (init_tile Tee Blue))) Y)).corners));
  "turn_back"     >:: (fun _ -> assert_equal true (compare_lsts tee_corns
                                                     (turn_tile (turn_tile (flip_tile (turn_tile (turn_tile (init_tile Tee Blue))) Y))).corners));

  (* Z Tile Tests *)
  "z_grid"        >:: (fun _ -> assert_equal grid_of_z (grid t1_z));
  "flip_Z_over_Y" >:: (fun _ -> assert_equal true (compare_lsts flip_Y_z_corns (flip_tile t1_z Y).corners));
  "flip_to_turn"  >:: (fun _ -> assert_equal true (compare_lsts flipturn_z_corns
                                                     (turn_tile (flip_tile (init_tile Z Blue) Y)).corners));
  "turn again"    >:: (fun _ -> assert_equal true (compare_lsts flip_Y_z_corns
                                                     (turn_tile (turn_tile (flip_tile (init_tile Z Blue) Y))).corners));
  "flip over X"   >:: (fun _ -> assert_equal true (compare_lsts z_corns
                                                     (flip_tile (turn_tile (turn_tile (flip_tile (init_tile Z Blue) Y))) X).corners));

  (* Tree Tile Tests *)
  "turn_tree"     >:: (fun _ -> assert_equal true (compare_lsts turn1_tree (turn_tile t_tree).corners));
  "turnflip_tree" >:: (fun _ -> assert_equal true (compare_lsts turnflip_tree (flip_tile (turn_tile (init_tile Tree Blue)) X).corners));

]

(*************************UPDATING STATE TESTS***************************)
let state_tests = [
  "init_canvas1" >:: (fun _ -> assert_equal
                         [((-1,1),White);  ((0,1),White);  ((1,1),White);
                          ((-1,0),White);  ((0,0),White);  ((1,0),White);
                          ((-1,-1),White); ((0,-1),White); ((1,-1),White)] init.canvas1);
  "init_canvas2" >:: (fun _ -> assert_equal
                         [((-1,1),White);  ((0,1),White);  ((1,1),White);
                          ((-1,0),White);  ((0,0),White);  ((1,0),White);
                          ((-1,-1),White); ((0,-1),White); ((1,-1),White)] init.canvas2);

  "init_player_stat" >:: (fun _ -> assert_equal Start (init.curr_player).status);
  "init_player_stat2">:: (fun _ -> assert_equal Start (nth init.players 1).status);
  "init_curr_p" >:: (fun _ -> assert_equal player_1 (init.curr_player));

  (*Player 1 valid first move*)
  "st2_curr_p" >:: (fun _ -> assert_equal player_2 (st2.curr_player));
  "st2_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                            ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                            ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (st2.canvas1));
  "st2_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                            ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                            ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (st2.canvas2));
  "st2_p1_stat" >:: (fun _ -> assert_equal Play (nth st2.players 0).status);
  "st2_p2_stat" >:: (fun _ -> assert_equal Start (nth st2.players 1).status);
  "st2_board" >:: (fun _ -> assert_equal (Array.get (st2.board) 0) ((0,0),Yellow));
  "st2_p1_score" >:: (fun _ -> assert_equal 1 (nth st2.players 0).score);
  "st2_p2_score" >:: (fun _ -> assert_equal 0 (nth st2.players 1).score);

  (*Player 1 invalid first move*)
  "st2'_curr_p" >:: (fun _ -> assert_equal p1_invalid_move1 (st2_invalid_move.curr_player));
  "st2'_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (st2_invalid_move.canvas1));
  "st2'_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (st2_invalid_move.canvas2));
  "st2'_p1_stat" >:: (fun _ -> assert_equal Start (nth st2_invalid_move.players 0).status);
  "st2'_p2_stat" >:: (fun _ -> assert_equal Start (nth st2_invalid_move.players 1).status);
  "st2'_p1_score" >:: (fun _ -> assert_equal 0 (nth st2_invalid_move.players 0).score);
  "st2'_p2_score" >:: (fun _ -> assert_equal 0 (nth st2_invalid_move.players 1).score);
  "st2'_board" >:: (fun _ -> assert_equal (Array.get (st2_invalid_move.board) 255) ((15,15),White));

  (*Player 2 first move*)
  "st3_curr_p" >:: (fun _ -> assert_equal p1_move_1 (st3.curr_player));
  "st3_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (st3.canvas1));
  "st3_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (st3.canvas2));
  "st3_p1_stat" >:: (fun _ -> assert_equal Play (nth st3.players 0).status);
  "st3_p2_stat" >:: (fun _ -> assert_equal Play (nth st3.players 1).status);
  "st3_p1_score" >:: (fun _ -> assert_equal 1 (nth st3.players 0).score);
  "st3_p2_score" >:: (fun _ -> assert_equal 1 (nth st3.players 1).score);
  "st3_board" >:: (fun _ -> assert_equal (Array.get (st3.board) 255) ((15,15),Blue));

  (*Turn canvas command*)
  "st4_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),Yellow);
                                             ((-1,0),Yellow);  ((0,0),Yellow);  ((1,0),Yellow);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),Yellow)]
                        (init4'.canvas1));
  "st4_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (init4'.canvas2));
  "st4_p1_stat" >:: (fun _ -> assert_equal Start (nth init4'.players 0).status);
  "st4_p2_stat" >:: (fun _ -> assert_equal Start (nth init4'.players 1).status);
  "st4_p1_score" >:: (fun _ -> assert_equal 0 (nth init4'.players 0).score);
  "st4_p2_score" >:: (fun _ -> assert_equal 0 (nth init4'.players 1).score);

  (*Flip canvas command*)
  "st5_canvas1" >:: (fun _ -> assert_equal [((-1,1),Yellow);  ((0,1),Yellow);  ((1,1),Yellow);
                                             ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (init5'.canvas1));
  "st5_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (init5'.canvas2));
  "st5_p1_stat" >:: (fun _ -> assert_equal Start (nth init5'.players 0).status);
  "st5_p2_stat" >:: (fun _ -> assert_equal Start (nth init5'.players 1).status);
  "st5_p1_score" >:: (fun _ -> assert_equal 0 (nth init5'.players 0).score);
  "st5_p2_score" >:: (fun _ -> assert_equal 0 (nth init5'.players 1).score);

  (*Player 1 second move*)
  "st6_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (init6'''.canvas1));
  "st3_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (init6'''.canvas2));
  "st3_p1_stat" >:: (fun _ -> assert_equal Play (nth init6'''.players 0).status);
  "st3_p2_stat" >:: (fun _ -> assert_equal Play (nth init6'''.players 1).status);
  "st3_p1_score" >:: (fun _ -> assert_equal 5 (nth init6'''.players 0).score);
  "st3_p2_score" >:: (fun _ -> assert_equal 1 (nth init6'''.players 1).score);
  "st3_board" >:: (fun _ -> assert_equal (Array.get (init6'''.board) 17) ((1,1),Yellow));

  (*Player 2 second move - invalid*)
  "st7_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (init7_p2.canvas1));
  "st7_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),Blue);  ((1,0),White);
                                             ((-1,-1),Blue); ((0,-1),Blue); ((1,-1),Blue)]
                         (init7_p2.canvas2));
  "st7_p1_stat" >:: (fun _ -> assert_equal Play (nth init7_p2.players 0).status);
  "st7_p2_stat" >:: (fun _ -> assert_equal Play (nth init7_p2.players 1).status);
  "st7_p1_score" >:: (fun _ -> assert_equal 5 (nth init7_p2.players 0).score);
  "st7_p2_score" >:: (fun _ -> assert_equal 1 (nth init7_p2.players 1).score);
  "st7_status" >:: (fun _ -> assert_equal false init7_p2.game_over);

  (*Player 2 second move - valid*)
  "st8_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                             ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (init8_p2.canvas1));
  "st8_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                            ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                            ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                         (init8_p2.canvas2));
  "st8_p1_stat" >:: (fun _ -> assert_equal Play (nth init8_p2.players 0).status);
  "st8_p2_stat" >:: (fun _ -> assert_equal Play (nth init8_p2.players 1).status);
  "st8_p1_score" >:: (fun _ -> assert_equal 5 (nth init8_p2.players 0).score);
  "st8_p2_score" >:: (fun _ -> assert_equal 5 (nth init8_p2.players 1).score);
  "st8_status" >:: (fun _ -> assert_equal false init8_p2.game_over);

  (*FORFEIT*)
  "st9_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                            ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                            ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (init9_p1_f.canvas1));
  "st9_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                            ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                            ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (init9_p1_f.canvas2));
  "st9_p1_stat" >:: (fun _ -> assert_equal Stop (nth init9_p1_f.players 0).status);
  "st9_p2_stat" >:: (fun _ -> assert_equal Stop (nth init9_p1_f.players 1).status);
  "st9_p1_score" >:: (fun _ -> assert_equal 1 (nth init9_p1_f.players 0).score);
  "st9_p2_score" >:: (fun _ -> assert_equal 1 (nth init9_p1_f.players 1).score);
  "st9_status" >:: (fun _ -> assert_equal true init9_p1_f.game_over);


]

let suite = "State Test Suite" >::: flatten [tests;state_tests]
let _ = run_test_tt_main suite
