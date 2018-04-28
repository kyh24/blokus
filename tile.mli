
type tile_id = One | Tee | L | X | Z | Tree | Line

type color = White | Blue | Yellow

type tile = {
  name : tile_id;
  col : color;
  value : int;
  mutable grid: (int * int * color) list;
type tile_id =
  (* | One | Square | Xshape | Tee | Line | LilL | BigL *)
  | One | Tee | L | X | Z | Tree | Line
  (* i love foster with all my heart <3 blokaml *)

type color = White | Blue | Yellow

(*[tile] is a type representing a tile in the game*)
type tile = {
  name : tile_id;
  col : color;
  value : int;
  mutable grid: (int * int * color) list
}

(*[direction] represents the axis the player wants flip a tile over*)
type direction = X | Y

(*[init_tile id] is the initial state of tile t*)
val init_tile : tile_id -> color ->  tile

(*[tile_name t] is the name of tile t*)
val tile_name : tile -> tile_id

(*[value t] is the value of tile t*)
val value : tile -> int

(*[color t] is the color of tile t*)
val color : tile -> color

(*[color t] is the grid of tile t*)
val grid: tile -> (int * int * color) list

}

type direction = X | Y

let init_tile id c =
  match id with
  |One -> {name = One;
           col = c;
           value = 1;
           grid = [(-1,1,White);  (0,1,White);  (1,1,White);
                    (-1,0,White);  (0,0,c);      (1,0,White);
                    (-1,-1,White); (0,-1,White); (1,-1,White);]
          }
  |Tee -> {name = Tee;
           col= c;
           value = 5;
           grid = [(-1,1,c);      (0,1,c);  (1,1,c);
                    (-1,0,White);  (0,0,c);  (1,0,White);
                    (-1,-1,White); (0,-1,c); (1,-1,White);]
          }
  |L -> {name = L;
         col = c;
         value = 5;
         grid = [(-1,1,c);  (0,1,White);  (1,1,White);
                  (-1,0,c);  (0,0,White);  (1,0,White);
                  (-1,-1,c); (0,-1,White); (1,-1,c);]
        }
  |X -> {name = X;
         col = c;
         value = 5;
         grid = [(-1,1,White);  (0,1,c);  (1,1,White);
                  (-1,0,c);      (0,0,c);  (1,0,c);
                  (-1,-1,White); (0,-1,c); (1,-1,White);]
        }
  |Z -> {name = Z;
         col = c;
         value = 5;
         grid = [(-1,1,c);      (0,1,c);  (1,1,White);
                  (-1,0,White);  (0,0,c);  (1,0,White);
                  (-1,-1,White); (0,-1,c); (1,-1,c);]
        }
  |Tree -> {name = Tree;
            col = c;
            value = 5;
            grid = [(-1,1,c);      (0,1,c);  (1,1,White);
                     (-1,0,White);  (0,0,c);  (1,0,c);
                     (-1,-1,White); (0,-1,c); (1,-1,White);]
           }
  |Line -> {name = Line;
            col = c;
            value = 3;
            grid = [(-1,1,White);  (0,1,c);  (1,1,White);
                     (-1,0,White);  (0,0,c);  (1,0,White);
                     (-1,-1,White); (0,-1,c); (1,-1,White);]
           }

let tile_name t = t.name

let value t = t.value

let color t = t.col

let grid t = t.grid
