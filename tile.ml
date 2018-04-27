
type tile_id = One | Tee | L | X | Z | Tree | Line

type color = White | Blue | Yellow

type tile = {
  name : tile_id;
  col : color;
  value : int;
  mutable grid: (int * int * color) list;
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

let flip_tile t dir =
  let old_grid = grid t in
  let new_grid = List.map (fun (x,y,_) ->
  let coord =
    begin
    match dir with
    |X -> List.find (fun (a,b,_) -> a=x && b=(-y)) old_grid
    |Y -> List.find (fun (a,b,_) -> a=(-x) && b=y) old_grid
  end in
  let new_c = match coord with (_,_,c) -> c in
  (x,y,new_c)) old_grid in
  t.grid <- new_grid

let turn_tile t =
  let old_grid = grid t in
  let new_grid = List.map (fun (x,y,_) ->
  let coord = List.find (fun (a,b,_) -> a= (-y) && b=x) old_grid in
  let new_c = match coord with (_,_,c) -> c in
  (x,y,new_c)) old_grid in
  t.grid <- new_grid
