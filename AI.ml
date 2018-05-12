open Player
open Board
open Tile
open Command

type turn =
  {center_pos : int;
   chosen_tile: tile;
  }

let tiles = [One; Line; Tree; Tee; X; Z; L]

(* [random_tile brd] returns a random, untried tiel from the player [p]'s, that is,
   the AI's, remaining pieces

   val random_tile : board -> player -> tile
*)
let rec random_tile p tried_tiles =
  if p.max_val = 0 then Empty
  else if p.max_val = 1 then
    if List.exists (fun elt -> elt.name = One) tried_tiles then Empty
    else List.find (fun elt -> elt.name = One) p.remaining_tiles
  else if p.max_val = 3 then
    begin
      let three_tile = List.find (fun elt -> elt.name = Line) p.remaining_tiles in
      if List.mem three_tile tried_tiles then  (p.max_val <- 1; random_tile p (three_tile :: tried_tiles))
      else three_tile
    end
  else
    begin
      let five_tiles = List.filter (fun elt -> elt.value = 5 && not (List.mem elt tried_tiles)) p.remaining_tiles in
      if List.length five_tiles = 0 then (p.max_val <- 3; random_tile p tried_tiles)
      else
        begin
          let num_tiles = List.length five_tiles in
          let index = Random.int (num_tiles + 1) in
          List.nth five_tiles index
        end
    end

(*[possible_positions brd p t] returns a list of all the possible places that
  player [p] can place a tile t

  val possible_positions : board -> (int * int) list
*)
let possible_positions brd p =
  if p.status = Start then
    let size = brd_size brd in
    [(size - 1, size - 1); (size - 1, size - 2); (size - 2 , size - 1); (size -2, size - 2)]
  else
    let remaining_tile_names = List.map (fun elt -> elt.name) p.remaining_tiles in
    let placed_tile_names = List.filter (fun elt -> List.exists (fun elt' -> elt' <> elt) remaining_tile_names) tiles in
    
