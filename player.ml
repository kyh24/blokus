open Tile

let init_player n c turnval=
  {name = n;
   color = c;
   score = 0;
   first_turn = true;
   remaining_tiles = [init_tile One c , init_tile Tee c,
                      init_tile L c, init_tile X c,
                      init_tile Z c, init_tile Tree c,
                      init_tile Line c]
  }

let name p =
  p.name

let color p =
  p.color

let score p =
  p.score

let remaining_tiles p =
  p.remaining_tiles
