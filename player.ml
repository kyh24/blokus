open Tile

type status = Start | Play | Stop

type player = {
  player_name : string;
  col : color;
  mutable status : status;
  mutable score : int;
  mutable remaining_tiles : tile list;
}


let init_player n c =
  {player_name = n;
   col = c;
   score = 0;
   status = Start;
   remaining_tiles = [init_tile One c; init_tile Tee c;
                      init_tile L c; init_tile X c;
                      init_tile Z c; init_tile Tree c;
                      init_tile Line c; init_tile Two c;
                      init_tile Four c; init_tile Boot c;
                      init_tile Kink c; init_tile C c;
                      init_tile Cowgirl c; init_tile Couch c;
                     init_tile Recliner c;init_tile Stairmaster c]}

let name p =
  p.player_name

let color p =
  p.col

let score p =
  p.score

let status p =
  p.status

let remaining_tiles p =
  p.remaining_tiles

let player_place_tile p t =
  p.score <- p.score + t.value;
  p.remaining_tiles <- List.filter (fun elt -> tile_name elt != tile_name t) p.remaining_tiles;
