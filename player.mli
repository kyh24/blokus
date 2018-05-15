open Tile

(* [status] represents the player status of whether they are doing their
 * first move, successive move, or finally forfeit/stop.*)
type status = Start | Play | Stop

(*[player] is an abstract type representing the player of the game.
 * Includes the player's name, their color, status in game play, score,
 * remaining tiles/inventory. *)
type player = {
  player_name : string;
  col : color;
  mutable status : status;
  mutable score : int;
  mutable remaining_tiles : tile list;
}

(*[init player n c t_lst] initializes the player when given a string for
 * the player name and a color.  *)
val init_player: string -> color -> player

(*[name p] is the name of the player.*)
val name: player -> string

(*[color p] is the color of the player's tiles. Each player can only be represented by
  one color chosen at the start of the game.*)
val color: player -> color

(*[score p] is the player's current score.*)
val score: player -> int

(*[status p] is the player's current status in the game.*)
val status: player -> status

(*[remaining_tiles p] is the list of tiles that the player has not yet placed
  onto the board*)
val remaining_tiles : player -> tile list

(*[player_place_tile p t] removes tile [t] from player [p]'s list of remaining tiles*)
val player_place_tile: player -> tile -> unit
