open Tile
open Array

type board= ((int * int) * color) array

(*[init_row brd_size x y] intializes a row [y] of a board of size [brd_size] with the color white*)
let rec init_row brd_size x (y:int) =
  if brd_size = 0 then []
  else ((x,y), White)::(init_row (brd_size - 1) (x+1) y)

(*[init_board_lst brd_size iterx itery] initalizes a board as a list with all white cells*)
let rec init_board_lst brd_size iterx itery =
  if itery = brd_size then []
  else (init_row (brd_size) 0 itery)@(init_board_lst brd_size (itery+1) (iterx +1))

let init_board brd_size = of_list (init_board_lst brd_size 0 0)

let brd_size brd = int_of_float (float_of_int (length brd) ** (1./.2.))

let get_index (x,y) size = y*size + x

let rec place_tile_on_brd tile_colors brd =
  match tile_colors with
  |[] -> ()
  |((x,y), c)::t -> begin
      let size = brd |> brd_size in
      let i = size |> get_index (x,y) in
      set brd i ((x,y), c); place_tile_on_brd t brd
    end
