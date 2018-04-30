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


let tests = [

  "tile_name"  >:: (fun _ -> assert_equal One (tile_name t1));
  (* "tile_col"  >:: (fun _ -> assert_equal Blue (col t)); *)
  "tile_grid"  >:: (fun _ -> assert_equal grid_t1 (grid t1));
  "tile_corners" >:: (fun _ -> assert_equal corners_t (t1.corners));

  "t_flipped_cornersX" >:: (fun _ -> assert_equal true (compare_lsts t1.corners ((flip_tile t1 X).corners)));
  "t_flipped_cornersY" >:: (fun _ -> assert_equal true (compare_lsts t2.corners ((flip_tile t2 Y).corners)));
  "t_turn1_corners" >:: (fun _ -> assert_equal true (compare_lsts corners_t ((turn_tile t3 ).corners)));









]

let suite = "State Test Suite" >::: tests
let _ = run_test_tt_main suite
