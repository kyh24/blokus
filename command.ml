open Tile
Open Graphics

(*[direction] represents the axis the player wants flip a tile over*)
type direction = X | Y

(* [command] represents a command input by a player. *)
type command = FLIP of direction | TURN of tile | PLACE of tile

(*[parse_click ()] is the command selected by a mouse event*)
let parse_click () = failwith "Unimplemented"

(*if we implement by having clickable commands on gui*)

let commands (c:unit) : command = if key_pressed c then
    let pos = mouse_pos c in
    if pos > (5,4) then FLIP
    else if pos > (0,0) then TURN
    else if pos > (1,1) then PLACE
    else failwith "poop"
(*if we implement using user input from terminal*)

print_string "Enter a command (flip, turn, place): ";
let cmd = read_line () in
let cmd_mod = lowercase_ascii cmd in
match cmd_mod with
|"flip" -> FLIP
|"turn" -> TURN
|"place" -> PLACE
| _ ->  Exception (*Printf.printf "%s%d\n" str poop*)
