open Tile
open Graphics

(*[direction] represents the axis the player wants flip a tile over*)
type direction = X | Y

(* [command] represents a command input by a player. *)
type command = FLIP of direction | TURN of tile | PLACE of tile

(*[parse_click ()] is the command selected by a mouse event*)
let parse_click () = failwith "Unimplemented"

(*if we implement by having clickable commands on gui*)

let commands (c:unit) (t:tile): command =
  if key_pressed c then (
    let pos = mouse_pos c in
    if pos > (5,4) then FLIP X
    else if pos > (0,0) then FLIP Y
<<<<<<< HEAD
    else if pos > (0,0) then TURN t
    else if pos > (1,1) then PLACE t
    else failwith "poop" ) else failwith "fail"
=======
    else if pos > (2,0) then TURN
    else if pos > (1,1) then PLACE
    else failwith "poop"
>>>>>>> 2634d042d3e9a1eb4f44bfa368dc69687e5dc20e
(*if we implement using user input from terminal*)

        (*
print_string "Enter a command (flip, turn, place): ";
let cmd = read_line () in
let cmd_mod = lowercase_ascii cmd in
match cmd_mod with
|"flip x" -> FLIP X
|"flip y " -> FLIP Y
|"turn" -> TURN
|"place" -> PLACE
| _ ->  Exception (*Printf.printf "%s%d\n" str poop*)
*)
