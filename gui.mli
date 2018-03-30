(* open Lwt *)
(* open GMain *)
(* open GObj *)
open Graphics

(* This module represents the GUI window which will display State
* changes depending on user commands and acceptable actions.
* The GUI will determine the right state depending on actions
* taken by the player that are acceptable by the program.
* Reference: https://ocaml.org/learn/tutorials/objects.html
* We are considering to also use Lwt, GMain, GObj to improve
* the functionality of the GUI rather than just using Graphics.
*)

(* [init_gui] initializes the GUI to the starting state.  The initial
 * state is specified in the State module.  This is the opening
 * GUI screen.*)
val init_gui: State.state ->unit

(* [update_state] will update the GUI display with a new state, depending
 * on the user's (valid) actions.  This will handle the user's movement of
 * the tile into an approporiate position.*)
val update_state: State.state -> unit

(* [build_board] will build the board, which is a grid of 8x8 grid.*)
val build_board: State.state -> int -> int -> unit

(* [get_command] will get the user's command or action.*)
val get_command: Command.command -> unit

(* [get_dim] returns the dimensions of the GUI window to properly format
* the game board.*)
val get_dim: int * int

(* [end_gui] ends the GUI to the starting state.  The initial
 * state is specified in the State module.*)
val end_gui: State.state -> unit

(* [draw_elts] will display the shapes and other elements such as the
 * shapes panel, the rotations table and the buttons.
 * needed in the GUI.*)
val draw_elts: State.state -> unit
