open Tile

type status = Start | Play | Stop

(*[player] is an abstract type representing the state of an adventure. *)
type player = {
  player_name : string;
  col : color;
  mutable status : status;
  mutable score : int;
  mutable remaining_tiles : tile list;
}

(*[init player n c t_lst] is the state of player [p] at the beginning of the game. *)
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
