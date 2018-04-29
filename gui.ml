(*GUI.ml
  See gui.mli for signatures
  Maintains the GUI frame which includes what elements must be a part of
  the application at all times. *)
open Tile
open Player
open State
open Command
open Graphics


(* let start_game_page =
  clear_graph ();
  draw_rect 200 200 200 200; *)

(* type gui_state_event =
  | Init
  | Comm of Command.command
  | State of State.state

let init_gui st = failwith "Unimplemented" *)

(* [update_state] will update the GUI display with a new state, depending
 * on the user's (letid) actions.  This will handle the user's movement of
 * the tile into an approporiate position.*)
(* let update_state st = failwith "Unimplemented" *)

(* [build_board] will build the board, which is a grid of 8x8 grid.*)
(* let build_board st i i = failwith "Unimplemented" *)

(* [get_command] will get the user's command or action.*)
(* let get_command c = failwith "Unimplemented" *)

(* [end_gui] ends the GUI to the starting state.  The initial
 * state is specified in the State module.*)
(* let end_gui st = () *)

(* [draw_elts] will display the shapes and other elements such as the
 * shapes panel, the rotations table and the buttons.
 * needed in the GUI.*)
(* let draw_elts st = () *)
