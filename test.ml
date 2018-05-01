open OUnit2
open State
open Tile

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
let flip_X_tee_corns = [(-1,2);(1,2);(2,0);(2,-2);(-2,-2);(2,0)]
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

let tests = [

  (* One Tile Tests  *)
  "tile_name"     >:: (fun _ -> assert_equal One (tile_name t1));
  "tile_col"      >:: (fun _ -> assert_equal Blue (col t1));
  "tile_grid"     >:: (fun _ -> assert_equal grid_t1 (grid t1));
  "tile_corners"  >:: (fun _ -> assert_equal corners_t (t1.corners));
  "tile_corners2" >:: (fun _ -> assert_equal true (compare_lsts t1.corners (flip_tile t1 X).corners));

  (* L Tile Tests *)
  "L_tile"        >:: (fun _ -> assert_equal L (tile_name t1_L));
  "flip L"        >:: (fun _ -> assert_equal true (compare_lsts flip_X_corners (flip_tile t1_L X).corners));
  "turn_from_flip">:: (fun _ -> assert_equal true
                          (((t1_L |> turn_tile).corners) |>
                           compare_lsts turn_flip_X));
  "turn_2"        >:: (fun _ -> assert_equal true (compare_lsts turn_2 (turn_tile t1_L).corners));
  "turn_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts corners_of_t1_L (turn_tile t1_L).corners));
  "flip_Y"        >:: (fun _ -> assert_equal true (compare_lsts flip_Y_corners (flip_tile t1_L Y).corners));

  (* X Tile Tests *)
  (* "X_tile"        >:: (fun _ -> assert_equal X (tile_name t1_x)); *)
  "flip X"        >:: (fun _ -> assert_equal true (compare_lsts x_Corners (flip_tile t1_x X).corners));
  "turn_from_flip">:: (fun _ -> assert_equal true
                          (((t1_x |> turn_tile).corners) |>
                           compare_lsts x_Corners));
  "turn_2"        >:: (fun _ -> assert_equal true (compare_lsts x_Corners (turn_tile t1_x).corners));
  "turn_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts x_Corners (turn_tile t1_x).corners));
  "flip_Y"        >:: (fun _ -> assert_equal true (compare_lsts x_Corners (flip_tile t1_x Y).corners));

  (* Line Tile Tests *)
  "flip L"        >:: (fun _ -> assert_equal true (compare_lsts line_corns (flip_tile t1_line X).corners));
  "flip_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts line_corns (flip_tile t1_line X).corners));
  "flip Y"        >:: (fun _ -> assert_equal true (compare_lsts line_corns (flip_tile t1_line Y).corners));
  "turn_from_flip">:: (fun _ -> assert_equal true
                          (((t1_line |> turn_tile).corners) |>
                           compare_lsts turn1_corns));
  "turn_2"        >:: (fun _ -> assert_equal true (compare_lsts line_corns (turn_tile t1_line).corners));
  "turn_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts turn1_corns (turn_tile t1_line).corners));

  (* Tee Tile Tests *)
  "flip L"        >:: (fun _ -> assert_equal true (compare_lsts turn1_tee_corns (turn_tile t1_tee).corners));
  "flip_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts flip_X_tee_corns (turn_tile t1_tee).corners));
  "flip Y"        >:: (fun _ -> assert_equal true (compare_lsts flip_X_tee_corns (flip_tile t1_tee Y).corners));
  "turn_from_flip">:: (fun _ -> assert_equal true
                          (((X |> flip_tile t1_tee).corners) |>
                           compare_lsts tee_corns));
  "turn_2"        >:: (fun _ -> assert_equal true (compare_lsts turn3_tee_corns (X |> flip_tile t1_tee |> turn_tile).corners));
  "turn_to_norm"  >:: (fun _ -> assert_equal true (compare_lsts tee_corns (turn_tile t1_tee).corners));

  (* Z Tile Tests *)
  "z_grid"        >:: (fun _ -> assert_equal grid_of_z (grid t1_z));
  "flip_Z_over_Y" >:: (fun _ -> assert_equal true (compare_lsts flip_Y_z_corns (flip_tile t1_z Y).corners));
  "flip_to_turn"  >:: (fun _ -> assert_equal true (compare_lsts flipturn_z_corns (turn_tile t1_z).corners));
  "turn again"    >:: (fun _ -> assert_equal true (compare_lsts flip_Y_z_corns (turn_tile t1_z).corners));
  "flip over X"   >:: (fun _ -> assert_equal true (compare_lsts z_corns (flip_tile t1_z X).corners));

  (* Tree Tile Tests *)
  "turn_tree"     >:: (fun _ -> assert_equal true (compare_lsts turn1_tree (turn_tile t_tree).corners));
  "turnflip_tree" >:: (fun _ -> assert_equal true (compare_lsts turnflip_tree (flip_tile t_tree X).corners));





]

let suite = "State Test Suite" >::: tests
let _ = run_test_tt_main suite
