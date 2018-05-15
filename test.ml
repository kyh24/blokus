open OUnit2
open State
open Tile
open Player

let rec compare_lsts lst1 lst2 =
  match lst1 with
  | [] -> true
  | h::t -> List.mem h lst2 && compare_lsts t lst2

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

let player_1 = init_player "Player 1" Yellow
let player_2 = init_player "Player 2" Blue
let t1_init = init_tile One Yellow
let t2_init = init_tile One Blue
let t1' = init_tile Tee Yellow

(*st2*)
let st2 = do_command (PLACE ((404,570),t1_init.name)) init2'
let st2_invalid_move = do_command (PLACE ((404,570),t1_init.name)) init3

let player_1_2 = {player_name = "Player 1";
                  col = Yellow;
                  score = 1;
                  status = Play;
                  remaining_tiles = [init_tile Tee Yellow;
                                     init_tile L Yellow; init_tile X Yellow;
                                     init_tile Z Yellow; init_tile Tree Yellow;
                                     init_tile Line Yellow];
                 }
let player_1_3 = {player_name = "Player 1";
                  col = Yellow;
                  score = 1;
                  status = Play;
                  remaining_tiles = [init_tile Tee Yellow;
                                     init_tile L Yellow; init_tile X Yellow;
                                     init_tile Z Yellow; init_tile Tree Yellow;
                                     init_tile Line Yellow];
                 }
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
  "init_player_stat2">:: (fun _ -> assert_equal Start (List.nth init.players 1).status);
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
  "st2_p1_stat" >:: (fun _ -> assert_equal Play (List.nth st2.players 0).status);
  "st2_p2_stat" >:: (fun _ -> assert_equal Start (List.nth st2.players 1).status);
  "st2_board" >:: (fun _ -> assert_equal (Array.get (st2.board) 0) ((0,0),Yellow));
  "st2_p1_score" >:: (fun _ -> assert_equal 1 (List.nth st2.players 0).score);
  "st2_p2_score" >:: (fun _ -> assert_equal 0 (List.nth st2.players 1).score);

  (*Player 1 invalid first move*)
  "st2'_curr_p" >:: (fun _ -> assert_equal player_1 (st2_invalid_move.curr_player));
  "st2'_canvas1" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                            ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
                                            ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (st2_invalid_move.canvas1));
  "st2'_canvas2" >:: (fun _ -> assert_equal [((-1,1),White);  ((0,1),White);  ((1,1),White);
                                        ((-1,0),White);  ((0,0),White);  ((1,0),White);
                                        ((-1,-1),White); ((0,-1),White); ((1,-1),White)]
                        (st2_invalid_move.canvas2));
  "st2'_p1_stat" >:: (fun _ -> assert_equal Start (List.nth st2.players 0).status);
  "st2'_p2_stat" >:: (fun _ -> assert_equal Start (List.nth st2.players 1).status);
  "st2'_p1_score" >:: (fun _ -> assert_equal 0 (List.nth st2.players 0).score);
  "st2'_p2_score" >:: (fun _ -> assert_equal 0 (List.nth st2.players 1).score);
  "st2'_board" >:: (fun _ -> assert_equal (Array.get (st2.board) 0) ((0,0),Yellow));


  (*"st1" >:: (fun _ -> assert_equal
                {board = [|((0,0),White);((1,0),White);((2,0),White);((3,0),White);
                           ((4,0),White);((5,0),White);((6,0),White);((7,0),White);
                           ((0,1),White);((1,1),White);((2,1),White);((3,1),White);
                           ((4,1),White);((5,1),White);((6,1),White);((7,1),White);
                           ((0,2),White);((1,2),White);((2,2),White);((3,2),White);
                           ((4,2),White);((5,2),White);((6,2),White);((7,2),White);
                           ((0,3),White);((1,3),White);((2,3),White);((3,3),White);
                           ((4,3),White);((5,3),White);((6,3),White);((7,3),White);
                           ((0,4),White);((1,4),White);((2,4),White);((3,4),White);
                           ((4,4),White);((5,4),White);((6,4),White);((7,4),White);
                           ((0,5),White);((1,5),White);((2,5),White);((3,5),White);
                           ((4,5),White);((5,5),White);((6,5),White);((7,5),White);
                           ((0,6),White);((1,6),White);((2,6),White);((3,6),White);
                           ((4,6),White);((5,6),White);((6,6),White);((7,6),White);
                           ((0,7),White);((1,7),White);((2,7),White);((3,7),White);
                           ((4,7),White);((5,7),White);((6,7),White);((7,7),White)|];
   players = [player_1_2; player_2];
   canvas1 = [((-1,1),White);  ((0,1),White);  ((1,1),Yellow);
              ((-1,0),Yellow);  ((0,0),Yellow);  ((1,0),Yellow);
              ((-1,-1),White); ((0,-1),White); ((1,-1),Yellow)];
   canvas2 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
              ((-1,0),White);  ((0,0),White);  ((1,0),White);
              ((-1,-1),White); ((0,-1),White); ((1,-1),White)];
   curr_player = player_1_2;
                 game_over = false} (do_command (TURN t.name) init));

  "st2" >:: (fun _ -> assert_equal
               {board = [|((0,0),Yellow);((1,0),White);((2,0),White);((3,0),White);((4,0),White);
               ((5,0),White);((6,0),White);((7,0),White); ((8,0),White);((9,0),White);
               ((0,1),White);((1,1),White);((2,1),White);((3,1),White);((4,1),White);
               ((5,1),White);((6,1),White);((7,1),White); ((8,1),White);((9,1), White);
               ((0,2),White);((1,2),White);((2,2),White);((3,2),White);((4,2),White);
               ((5,2),White);((6,2),White);((7,2),White); ((8,2),White);((9,2), White);
               ((0,3),White);((1,3),White);((2,3),White);((3,3),White);((4,3),White);
               ((5,3),White);((6,3),White);((7,3),White); ((8,3),White); ((9,3), White);
               ((0,4),White);((1,4),White);((2,4),White);((3,4),White);((4,4),White);
               ((5,4),White);((6,4),White);((7,4),White); ((8,4),White); ((9,4), White);
               ((0,5),White);((1,5),White);((2,5),White);((3,5),White);((4,5),White);
               ((5,5),White);((6,5),White);((7,5),White); ((8,5),White); ((9,5), White);
               ((0,6),White);((1,6),White);((2,6),White);((3,6),White);((4,6),White);
               ((5,6),White);((6,6),White);((7,6),White); ((8,6),White); ((9,6), White);
               ((0,7),White);((1,7),White);((2,7),White);((3,7),White);((4,7),White);
               ((5,7),White);((6,7),White);((7,7),White); ((8,7),White); ((9,7), White);
               ((0,8),White);((1,8),White);((2,8),White);((3,8),White);((4,8),White);
               ((5,8),White);((6,8),White);((7,8),White); ((8,8),White); ((9,8), White);
               ((0,9),White);((1,9),White);((2,9),White);((3,9),White);((4,9),White);
               ((5,9),White);((6,9),White);((7,9),White); ((8,9),White); ((9,9), White);|];
    players = [player_1_3; player_2];
    canvas1 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
    ((-1,0),White);  ((0,0),Yellow);  ((1,0),White);
    ((-1,-1),White); ((0,-1),White); ((1,-1),White)];
    canvas2 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
    ((-1,0),White);  ((0,0),White);  ((1,0),White);
    ((-1,-1),White); ((0,-1),White); ((1,-1),White)];
    curr_player = player_2;
                game_over = false} (do_command (PLACE ((205, 570), One)) st2)); *)

]

let suite = "State Test Suite" >::: List.flatten [tests;state_tests]
let _ = run_test_tt_main suite
