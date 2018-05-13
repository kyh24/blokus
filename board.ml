open Tile


type board= ((int * int) * color) array

let rec init_row brd_size x (y:int) =
  if brd_size = 0 then []
  else ((x,y), White)::(init_row (brd_size - 1) (x+1) y)

let rec init_board_lst brd_size iterx itery =
  if  itery = brd_size then [] else (init_row (brd_size) 0 itery)@ (init_board_lst brd_size (itery+1) (iterx +1))

let init_board brd_size : ((int * int) * color) array =
  Array.of_list (init_board_lst brd_size 0 0)

let brd_size brd = int_of_float (float_of_int (Array.length brd) ** (1./.2.))

let get_index (x,y) size = y*size + x

let rec place_tile_on_brd tile_colors brd =
  match tile_colors with
  |[] -> ()
  |((x,y), c)::t -> if x < 0 || x > brd_size brd - 1|| y > brd_size brd -1 || y < 0 then place_tile_on_brd t brd
    else Array.set brd (get_index (x,y) (brd_size brd)) ((x,y), c); place_tile_on_brd t brd

  
