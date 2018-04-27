
type color = White | Blue | Yellow

module Board = struct
  type board= ((int * int) * color) array
  type gridonboard = ((int * int) * color) list

  let init_board =
    [|((0,0),Blue) ; ((1,0),White); ((2,0),White); ((3,0),White); ((4,0),White); ((5,0),White); ((6,0),White); ((7,0),White);
      ((0,1),White); ((1,1),White); ((2,1),White); ((3,1),White); ((4,1),White); ((5,1),White); ((6,1),White); ((7,1),White);
      ((0,2),White); ((1,2),White); ((2,2),White); ((3,2),White); ((4,2),White); ((5,2),White); ((6,2),White); ((7,2),White);
      ((0,3),White); ((1,3),White); ((2,3),White); ((3,3),White); ((4,3),White); ((5,3),White); ((6,3),White); ((7,3),White);
      ((0,4),White); ((1,4),White); ((2,4),White); ((3,4),White); ((4,4),White); ((5,4),White); ((6,4),White); ((7,4),White);
      ((0,5),White); ((1,5),White); ((2,5),White); ((3,5),White); ((4,5),White); ((5,5),White); ((6,5),White); ((7,5),White);
      ((0,6),White); ((1,6),White); ((2,6),White); ((3,6),White); ((4,6),White); ((5,6),White); ((6,6),White); ((7,6),White);
      ((0,7),White); ((1,7),White); ((2,7),White); ((3,7),White); ((4,7),White); ((5,7),White); ((6,7),White); ((7,7),Yellow);
    |]

  (* let get_center_cell plane pos =
    Array.get plane pos

  let gridonboard centcoord  =
    let ((x,y), color) = centcoord in
    { value =  4;
      grid = [| ((),)
             |]
    } *)

end
