open Graphics
open Printf
open State
open Player
open Board
open Command
open Tile

(* This module represents the GUI window which will display State
 * changes depending on user commands and acceptable actions.
 * The GUI will draw the view of the contents of the window,
 * modify state and other fields depending on user actions, and process
 * new displays through a REPL.
 *)

(* [opening] initializes the GUI to the starting state.  The initial screen
 * is the game opening screen, which is in gui.ml.  This function handles
 * shifting to functions that handle the board screen and use/update state.
 *)

val opening: unit -> unit
