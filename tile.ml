
type tile_id = One | Tee | L | X | Z | Tree | Line

type color = White | Blue | Yellow

type tile = {
  name : tile_id;
  col : color;
  value : int;
  mutable grid: ((int * int) * color) list;
  mutable corners: (int *int) list;
}

type direction = X | Y

let init_tile id c =
  match id with
  |One -> {name = One;
           col = c;
           value = 1;
           grid = [((-1,1),White);  ((0,1),White);  ((1,1),White);
                   ((-1,0),White);  ((0,0),c);      ((1,0),White);
                   ((-1,-1),White); ((0,-1),White); ((1,-1),White)];
           corners = [(-1,1);(1,1);(-1,-1);(1,-1)]
          }
  |Tee -> {name = Tee;
           col= c;
           value = 5;
           grid = [((-1,1),c);      ((0,1),c);  ((1,1),c);
                   ((-1,0),White);  ((0,0),c);  ((1,0),White);
                   ((-1,-1),White); ((0,-1),c); ((1,-1),White)];
           corners = [(-2,2);(2,2);(-1,-2);(1,-2);(2,0);(-2,0)]
          }
  |L -> {name = L;
         col = c;
         value = 5;
         grid = [((-1,1),c);  ((0,1),White);  ((1,1),White);
                 ((-1,0),c);  ((0,0),White);      ((1,0),White);
                 ((-1,-1),c); ((0,-1),c); ((1,-1),c)];
         corners = [(-2,2);(0,2);(-2,-2);(2,-2);(2,0)]
        }
  |X -> {name = X;
         col = c;
         value = 5;
         grid = [((-1,1),White);  ((0,1),c);  ((1,1),White);
                 ((-1,0),c);  ((0,0),c);      ((1,0),c);
                 ((-1,-1),White); ((0,-1),c); ((1,-1),White)];
         corners = [(-1,2);(1,2);(2,1);(2,-1);(-2,1);(-2,-1);(-1,-2);(1,-2)]
        }
  |Z -> {name = Z;
         col = c;
         value = 5;
         grid = [((-1,1),c);  ((0,1),c);  ((1,1),White);
                 ((-1,0),White);  ((0,0),c);      ((1,0),White);
                 ((-1,-1),White); ((0,-1),c); ((1,-1),c)];
         corners = [(-2,2);(-2,0);(1,2);(-1,-2);(2,0);(2,-2)]
        }
  |Tree -> {name = Tree;
            col = c;
            value = 5;
            grid = [((-1,1),c);  ((0,1),c);  ((1,1),White);
                    ((-1,0),White);  ((0,0),c);      ((1,0),c);
                    ((-1,-1),White); ((0,-1),c); ((1,-1),White)];
            corners = [(-2,2);(-2,0);(1,2);(2,1);(2,-1);(1,-2);(-1,-2)]
           }
  |Line -> {name = Line;
            col = c;
            value = 3;
            grid = [((-1,1),White);  ((0,1),c);  ((1,1),White);
                    ((-1,0),White);  ((0,0),c);      ((1,0),White);
                    ((-1,-1),White); ((0,-1),c); ((1,-1),White)];
            corners = [(-1,2);(1,2);(1,-2);(-1,-2)]
           }

let tile_name t = t.name

let value t = t.value

let col t = t.col

let grid t = t.grid

let rec compare_lsts lst1 lst2 =
  match lst1 with
  | [] -> true
  | h::t -> List.mem h lst2 && compare_lsts t lst2

let flip_tile t dir =
  let new_grid = List.map (fun ((x,y),_) ->
      let coord =
        begin
          match dir with
          |X -> List.find (fun ((a,b),_) -> a=x && b=(-y)) t.grid
          |Y -> List.find (fun ((a,b),_) -> a=(-x) && b=y) t.grid
        end in
      let new_c = match coord with ((_,_),c) -> c in
      ((x,y),new_c)) t.grid in
  t.grid <- new_grid;
  t.corners <-
    begin
      match dir with
      |X-> List.map(fun (x,y) -> (x,-y)) t.corners
      |Y -> List.map(fun (x,y) -> (-x,y)) t.corners
    end;
  t

let turn_tile t =
  let new_grid = List.map (fun ((x,y),_) ->
      let coord = List.find (fun ((a,b),_) -> a= (-y) && b=x) t.grid in
      let new_c = match coord with ((_,_),c) -> c in
      ((x,y),new_c)) t.grid in
  t.grid <- new_grid;
  t.corners <- List.map(fun (x,y) -> (y,-x)) t.corners;
  t
