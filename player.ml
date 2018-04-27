open Tile

<<<<<<< HEAD
type player = {
  name : string;
  col : color;
  mutable score : int;
  mutable remaining_tiles : tile list
}


let init_player n c s t_lst =
=======
let init_player n c turnval=
>>>>>>> 20675d94c376fffc9ef4ddc5f8fc1041313173be
  {name = n;
   col = c;
   score = 0;
<<<<<<< HEAD
   remaining_tiles = [init_tile One c; init_tile Tee c; init_tile L c;
                      init_tile X c; init_tile Z c; init_tile Tree c;
=======
   first_turn = true;
   remaining_tiles = [init_tile One c , init_tile Tee c,
                      init_tile L c, init_tile X c,
                      init_tile Z c, init_tile Tree c,
>>>>>>> 20675d94c376fffc9ef4ddc5f8fc1041313173be
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
