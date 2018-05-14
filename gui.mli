
(* This module represents the GUI window which will display State
 * changes depending on user commands and acceptable actions.
 * The GUI will determine the right state depending on actions
 * taken by the player that are acceptable by the program.
 *)

(* [opeing] initializes the GUI to the starting state.  The initial screen
 * is the game opening screen, which is in gui.ml.  This function handles
 * shifting to functions that handle the board screen and use/update state.
 *)

val opening: unit -> unit
