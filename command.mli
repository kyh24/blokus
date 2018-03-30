Open Tile

(*[direction] represents the axis the player wants flip a tile over*)
type direction = X | Y

(* [command] represents a command input by a player. *)
type command = FLIP of direction | TURN of tile | PLACE of tile

(*[parse_click ()]*)
val parse_click : unit -> command
