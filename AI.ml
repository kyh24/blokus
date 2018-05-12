open Player
open Board
open Tile
open Command

type turn =
  {center_pos : int;
   chosen_tile: tile;
  }

(* [random_tile brd] returns a random from the player [p]'s, that is,
   the AI's, remaining pieces

   val random_tile : board -> player -> tile
*)
let random_tile brd p =
  let num_tiles = List.length p.remaining_tiles in
  let index = Random.int num_tiles + 1 in
  let selected_tile = List.nth p.remaining_tiles index in

(* [smart_random_tile brd] returns a random tile from the player [p]'s, that is,
   the AI's, remaining pieces

   val smart_random_tile : board -> player -> tile
*)
  let smart_random_tile brd p =





(*[possible_positions brd p t] returns a list of all the possible places that
  player [p] can place a tile t

val possible_positions : board -> (int * int) list
*)
let possible_positions brd p t =
  if p.first_turn then
