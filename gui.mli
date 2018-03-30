open Lwt
open Graphics
open GMain
open GObj

(* [gui] represents the GUI window which will display State
 * changes depending on user commands and acceptable actions.
 * The GUI will determine the right state depending on actions
 * taken by the player that are acceptable by the program.
 * Reference: https://ocaml.org/learn/tutorials/objects.html
 *)
class gui:
  (* [update_state] will update the GUI display with a new state.
   * The *)
  method update_state: State.state -> unit

  (* [get_command] will get the user's command or action.*)
  method get_command: unit

  (* [get_dim] returns the dimensions of the GUI window to prpoerly format
   * the game board.*)
  method get_dim: int * int
end
(* [init_gui] initializes the GUI to the starting state *)
val init_gui: gui ->unit

(* [end_gui] ends the game.*)
val end_gui: gui ->unit

(* [draw_elts] will display the shapes and other elements
 * needed in the GUI.*)
val draw_elts: gui -> unit

(* [get_dim] returns the dimensions of the GUI window to prpoerly format
 * the game board.*)
val get_window_dim: gui -> int * int
