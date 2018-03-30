(* [verb] represents all the possible verb inputs by a player. *)
type verb = FLIP | TURN | PLACE

(* [command] represents a command input by a player. *)
type command = {verb: verb; obj: string}

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the following commands forms :
        "flip [direction]" where direction is either "x" or "y"
        "place [tile_name]" where tilename is a valid tile name
          in the game
        "turn [tile_name]" where tilename is a valid tile tile_name
          in the game    
*)
val parse : string -> command
