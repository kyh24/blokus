open Player
open Tile
open Board
open Command

(*[state] represents the state of the game*)
type state = {
  board : ((int*int) * color) array;
  players : player list;
  mutable canvas1: ((int*int)* color) list;
  mutable canvas2: ((int*int)* color) list;
  mutable curr_player: player;
  mutable game_over : bool
}

(*[init_state] is the initial state of the game with dimensions i by i*)
val init_state : int -> state

(* [do_command c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
*)
val do_command : command -> state -> state

(* [print_winner st] prints the win message if either of the players has won the
   game. prints a tie message if both players have status "Stop" with the same
   score.*)
val print_winner : state -> string
