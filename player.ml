open Tile

type player = {
  name : string;
  col : color;
  mutable first_turn : bool;
  mutable score : int;
  mutable remaining_tiles : tile list
}


let init_player n c =
  {name = n;
   col = c;
   score = 0;
   first_turn = true;
   remaining_tiles = [init_tile One c; init_tile Tee c;
                      init_tile L c; init_tile X c;
                      init_tile Z c; init_tile Tree c;
                      init_tile Line c]
  }

let name p =
  p.name

let color p =
  p.col

let score p =
  p.score

let remaining_tiles p =
  p.remaining_tiles
