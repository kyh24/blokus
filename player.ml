Open Tile

type player = {
  name : string;
  color : string;
  score : int;
  remaining_tiles : tile list
}

(*[init player n c t_lst] is the player at the beginning of the game*)
val init_player: string -> string -> tile list -> player

(*[name p] is the player at the beginning of the game*)
val name: player -> name

val color: player -> color

val score: player -> int

val remaining_tiles : player -> string list

val place_tile : player -> player
