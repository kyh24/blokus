open Tile

let init_player n c s t_lst =
  {name = n; color = c; score = s; remaining_tiles = t_lst}

let name p =
  p.name

let color p =
  p.color

let score p =
  p.score

let remaining_tiles p =
  p.remaining_tiles
