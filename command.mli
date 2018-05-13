open Tile

(*[direction] represents the axis the player wants flip a tile over*)
type direction = X | Y

(* [command] represents a command input by a player. *)
type command = FLIPX of tile_id | FLIPY of tile_id | TURN of tile_id | PLACE of (int * int) * tile_id | FORFEIT
