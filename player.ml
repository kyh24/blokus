open Tile

type player = {
  player_name : string;
  col : color;
  mutable first_turn : bool;
  mutable score : int;
  mutable remaining_tiles : tile list;
  mutable max_val: int
}


let init_player n c =
  {player_name = n;
   col = c;
   score = 0;
   first_turn = true;
   remaining_tiles = [init_tile One c; init_tile Tee c;
                      init_tile L c; init_tile X c;
                      init_tile Z c; init_tile Tree c;
                      init_tile Line c];
  max_val = 5
  }

let name p =
  p.player_name

let color p =
  p.col

let score p =
  p.score

let remaining_tiles p =
  p.remaining_tiles

let player_place_tile p t =
  p.score <- p.score + t.value;
  p.remaining_tiles <- List.filter (fun elt -> tile_name elt != tile_name t) p.remaining_tiles;
  let val_5_lst = List.filter (fun elt -> elt.value = 5) p.remaining_tiles in
  if List.length p.remaining_tiles = 0
  then p.max_val <- 0
  else if List.length val_5_lst = 0 && List.exists (fun elt -> elt.name = Line) p.remaining_tiles
  then p.max_val <- 3
  else if List.length val_5_lst = 0 && List.exists (fun elt -> elt.name = One) p.remaining_tiles
  then p.max_val <- 1
  else p.max_val <- 5
   
