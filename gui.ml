open Graphics
open Printf
open State
open Player
open Board
open Tile

type gamescreen = {
  mutable state: state; (*state.init_state 10*)
  mutable p1messages: string;
  mutable p2messages: string;
  mutable p1inventory: tile list;
  gwindow: int*int;
  gboard: int*int*int*int; (*200, 175 ,400, 400*)
  
  gp1rti: int *int *int *int;
  gp1fx: int* int* int* int;
  gp1fy: int*int*int*int;
  gp1rot: int*int*int*int;
  gp1canvas: (int * (int*int*int*int)) list;
  mutable gp1inv: (int*int*int*int) list

}

let game = {
  state = State.init_state 10;
  gwindow = (1200, 750);
  gboard = (200, 175, 400, 400);
  p1messages = "";
  p2messages = "";
  p1inventory= (init_player "Player1" Yellow).remaining_tiles;
  gp1rti= 200,274,190,66;
  gp1fx= 200,208,95,66;
  gp1fy= 295,208,95,66;
  gp1rot= 200,142,190,66;
  gp1canvas= [(0, (0,0,0,0))];
  gp1inv = []
}

(*Player 1 Buttons*)
(*
draw_rect 10 10 ((xf-xboard)/2 - 20) 120; *)

(*Draws Rectangles*)
let draw_gui_rect x y w h color =
  set_color color;
  fill_rect x y w h

let draw_gui_button figure color=
  match figure with
  |(x,y,w,h) -> set_color color; draw_rect x y w h

let draw_gui_text x y str color =
  set_color color;
  moveto x y;
  draw_string (str)

let get_x fig =
  match fig with
  |(x,_) ->x

let get_x figure=
  match figure with
  |(x,_,_,_) -> x

let get_y fig=
  match fig with
  |(_,y) ->y

let get_y figure=
  match figure with
  |(_,y,_,_) -> y

let get_w figure=
  match figure with
  |(_,_,w,_) -> w

let get_h figure=
  match figure with
  |(_,_,_,h) -> h

  let rec draw_tiles_helper tilelist i=
    match tilelist with
    | [] -> set_color black;
    | h::t ->
      if i=0 then set_color yellow else set_color cyan;
      begin
          match h.name with
          | One ->  fill_rect (30 +(800*i)) 680 30 30; draw_tiles_helper t i;
          | Tee ->  fill_rect (120+(800*i)) 680 30 30;
                    fill_rect (150+(800*i)) 680 30 30;
                    fill_rect (180+(800*i)) 680 30 30;
                    fill_rect (150+(800*i)) 650 30 30;
                    fill_rect (150+(800*i)) 620 30 30; draw_tiles_helper t i;
          | L ->    fill_rect (270 +(800*i)) 680 30 30;
                    fill_rect (270 +(800*i)) 650 30 30;
                    fill_rect (270 +(800*i)) 620 30 30;
                    fill_rect (300 +(800*i)) 620 30 30;
                    fill_rect (330 +(800*i)) 620 30 30; draw_tiles_helper t i;
          | X ->    fill_rect (60 +(800*i)) 560 30 30;
                    fill_rect (60 +(800*i)) 530 30 30;
                    fill_rect (60 +(800*i)) 500 30 30;
                    fill_rect (30 +(800*i)) 530 30 30;
                    fill_rect (90 +(800*i)) 530 30 30; draw_tiles_helper t i;
          | Z ->    fill_rect (150+(800*i)) 560 30 30;
                    fill_rect (180+(800*i)) 560 30 30;
                    fill_rect (180+(800*i)) 530 30 30;
                    fill_rect (180+(800*i)) 500 30 30;
                    fill_rect (210+(800*i)) 500 30 30; draw_tiles_helper t i;
          | Tree -> fill_rect (270+(800*i)) 560 30 30;
                    fill_rect (300+(800*i)) 560 30 30;
                    fill_rect (300+(800*i)) 530 30 30;
                    fill_rect (300+(800*i)) 500 30 30;
                    fill_rect (330+(800*i)) 530 30 30; draw_tiles_helper t i;
          | Line -> fill_rect (150+(800*i)) 410 30 30;
                    fill_rect (180+(800*i)) 410 30 30;
                    fill_rect (210+(800*i)) 410 30 30; draw_tiles_helper t i;
        end

  let rec draw_tiles playerlist=
    let playerarray= Array.of_list playerlist in
    for i=0 to 1
    do
      let tilelist= (playerarray.(i)).remaining_tiles in
      draw_tiles_helper tilelist i
    done

let clicker =
  let cl= Graphics.wait_next_event [Button_down] in
  (cl.mouse_x, cl.mouse_y)

(* let inv_click () =
  let (px,py)= clicker () in
  let rec which_button lst=
    match lst with
    | [] -> stor.message <- stor.message;
    | (s,c,(x,y,w,h))::t -> if (px>=x && px<=(x+w)) && (py>=y && py<=(y+h))
      then
        begin
           stor.message <- "You Clicked on "^s^"!";
        end
      else
        which_button t; *)

                  (*******************************)
let rec loop () =
  clear_graph ();
  set_color black;

  (*Board setup*)
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  let xboard = 400 in
  let yboard = 400 in
  let xboardleftcorner= (xf-xboard)/2 in
  let yboardleftcorner= (yf-yboard)/2 in

  (*Board cell set up*)
  for x = 0 to 9
  do
    for y = 0 to 9 do
      let pt1 = (xboardleftcorner) + (yboard/10 * (x)) in
      let pt2 = (yboardleftcorner +400) - (xboard/10 * (y+1)) in
      draw_rect pt1 pt2 (xboard/10) (yboard/10);
      moveto pt1 pt2; draw_string ("("^(string_of_int x)^", "^(string_of_int y)^")");
    done;
  done;

  (*Player Inventory set up*)
  (*Player 1 Inventory*)
  draw_rect 10 (yf- 400) ((xf-xboard)/2 - 20) 390;
  moveto 150 (yf-30); draw_string ("Player 1 Inventory");
  (*Player 2 Inventory*)
  draw_rect (xboardleftcorner + xboard + 10) (yf- 400) ((xf-xboard)/2 - 20) 390;
  moveto (xf-250) (yf-30); draw_string ("Player 2 Inventory");
  (*Player Canvas set up*)
  (*Player 1 Canvas*)
  for x = 1 to 3 do
    for y = 1 to 3 do
      let pt1 = 10 + ((((xf-xboard)/4 - 20)/3) * (x-1)) in
      let pt2 = (yf- 610) + ((200/3) * (y-1)) +2 in
      draw_rect pt1 pt2 (((xf-xboard)/4 - 20)/3) (200/3);
      moveto pt1 pt2; draw_string ("P1("^(string_of_int x)^", "^(string_of_int y)^")");
    done;
  done;



  (*ONE*)
  (* let one_1 (var, x_val, y_val) = fill_rect (x_val) (y_val) var var in
  (*TEE*)
  let tee_1 (var, x_val, y_val) = fill_rect (x_val) (y_val) var var; fill_rect (x_val+var) (y_val) var var; fill_rect (x_val+2*var) (y_val) var var; fill_rect (x_val+var) (y_val-var) var var; fill_rect (x_val+var) (y_val-2*var) var var in
  (*L*)
  let l_1 (var, x_val, y_val) =  fill_rect (x_val) (y_val) var var; fill_rect (x_val) (y_val-var) var var; fill_rect (x_val) (y_val-2*var) var var; fill_rect (x_val+var) (y_val-2*var) var var; fill_rect (x_val+2*var) (y_val-2*var) var var in
  (*X*)
  let x_1 (var, x_val, y_val) = fill_rect (x_val) (y_val) var var; fill_rect (x_val) (y_val-var) var var; fill_rect (x_val) (y_val-2*var) var var; fill_rect (x_val-var) (y_val-var) var var; fill_rect (x_val+var) (y_val-var) var var in
  (*Z*)
  let z_1 (var, x_val, y_val) =  fill_rect (x_val) (y_val) var var; fill_rect (x_val+var) (y_val) var var; fill_rect (x_val+var) (y_val-var) var var; fill_rect (x_val+var) (y_val-2*var) var var; fill_rect (x_val+2*var) (y_val-2*var) var var in
  (*TREE*)
  let tree_1 (var, x_val, y_val) = fill_rect (x_val) (y_val) var var; fill_rect (x_val+var) (y_val) var var; fill_rect (x_val+var) (y_val-var) var var; fill_rect (x_val+var) (y_val-2*var) var var; fill_rect (x_val+2*var) (y_val-var) var var in
  (*LINE*)
  let line_1 (var, x_val, y_val) = fill_rect (x_val) (y_val) var var; fill_rect (x_val+var) (y_val) var var; fill_rect (x_val+2*var) (y_val) var var in




  (*Player 1 Calling*)
  set_color yellow;
  one_1 (30, 30, 680);
  tee_1 (30, 120, 680);
  l_1 (30, 270, 680);
  x_1 (30, 60, 560);
  z_1 (30, 150, 560);
  tree_1 (30, 270, 560);
  line_1 (30, 150, 410);

  (*Player 2 Calling*)
  set_color blue;
  one_1 (30, 830, 680);
  tee_1 (30, 920, 680);
  l_1 (30, 1070, 680);
  x_1 (30, 860, 560);
  z_1 (30, 950, 560);
  tree_1 (30, 1070, 560);
  line_1 (30, 950, 410); *)

  (*Player 1 Buttons*)
  draw_gui_button game.gp1rti black;
  draw_gui_text 243 304 "Return to Inventory" black;

  draw_gui_button game.gp1fx black;
  draw_gui_text 230 238 "Flip X" black;

  draw_gui_button game.gp1fy black;
  draw_gui_text 325 238 "Flip Y" black;

  draw_gui_button game.gp1rot black;
  draw_gui_text 248 172 "Rotate Clockwise" black;

  draw_rect 10 10 ((xf-xboard)/2 - 20) 120;

  (*Player 2 Canvas *)
  (* draw_rect (xboard + (xf/3)+10 ) (yf- 610) ((xf-xboard)/4 - 20) 200; *)
  for x = 1 to 3 do
    for y = 1 to 3 do
      let pt1 = ((xboard + (xf/3)+10)) + ((((xf-xboard)/4 - 20)/3)*(x-1)) in
      let pt2 = (yf- 610) + ((200/3) * (y-1)) +2 in
      draw_rect pt1 pt2 (((xf-xboard)/4 - 20)/3) (200/3);
      moveto pt1 pt2; draw_string ("P2("^(string_of_int x)^", "^(string_of_int y)^")");
    done;
  done;

  draw_tiles game.state.players;

  (*Player 2 Buttons*)
  (* set_color blue; fill_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 476) (((xf-xboard)/4) -10) 66; *)
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 476) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto ((xboard + (xf/3)+((xf-xboard)/4))+43) (yf- 476 +30); draw_string ("Return to Inventory");

  draw_rect (xboard + (xf/3)+(xf-xboard)/4) (yf- 542) ((((xf-xboard)/4) -10)/2 ) 66;
  moveto (xboard + (xf/3)+((xf-xboard)/4)+ 30) (yf- 542 +30 ); draw_string ("Flip X");

  draw_rect (xboard + (xf/3)+((xf-xboard)/4)+(((((xf-xboard)/4) -10)/2))) (yf- 542) ((((xf-xboard)/4) -10)/2 ) 66;
  moveto (xboard + (xf/3)+((xf-xboard)/4)+125) (yf- 542 +30 ); draw_string ("Flip Y");

  (* set_color red; fill_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 608) (((xf-xboard)/4) -10) 66; *)
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 608) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto ((xboard + (xf/3)+((xf-xboard)/4))+48) (yf- 608 +30); draw_string ("Rotate Clockwise");

  draw_rect (xboard + (xf/3)+10 ) 10 ((xf-xboard)/2 - 20) 120;

  (*Messaage Board*)
  draw_rect xboardleftcorner 10 xboard ((yf-yboard)/2 -20);
(* returns the relative (x, y) of the next mouse click within the
 * grect defined by (x, y, w, h), doesn't terminate until the mouse is clicked
 * within the bounds of the window. *)






(* returns the relative (x, y) of the next mouse click within the
 * grect defined by (x, y, w, h), doesn't terminate until the mouse is clicked
 * within the bounds of the window. *)




let starting = wait_next_event [Key_pressed] in
if starting.key == 's' then loop () else ()


(* let e = wait_next_event [Mouse_motion] in
   let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in
   clear_graph ();
   moveto 0 0; draw_string mouse_description;
   if e.key <> 'q' then loop () else () *)


let rec opening ()=
  clear_graph ();
  (* Png.load ("blokaml.png"); *)
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  moveto (xf/2) (2*yf/3);
  set_color black; moveto ((xf/4)+((xf/2)/2)-20) ((yf/4)+(yf/7)/2); draw_string "Play Now!";
  moveto (xf/4) (2*yf/3);
  draw_string "  _  _  _  _    _  _                   _           _                 _           _  _";
  moveto (xf/4) ((2*yf/3)-10);
  draw_string "(_)(_)(_)(_) _(_)(_)                 (_)       _ (_)               (_) _     _ (_)(_)";
  moveto (xf/4) ((2*yf/3)-20);
  draw_string "(_)        (_)  (_)      _  _  _    (_)    _ (_)     _  _  _      (_)(_)   (_)(_)(_)";
  moveto (xf/4) ((2*yf/3)-30);
  draw_string "(_) _  _  _(_)  (_)   _ (_)(_)(_) _ (_) _ (_)       (_)(_)(_) _   (_) (_)_(_) (_)(_)";
  moveto (xf/4) ((2*yf/3)-40);
  draw_string "(_)(_)(_)(_)_   (_)  (_)         (_)(_)(_) _         _  _  _ (_)  (_)   (_)   (_)(_)";
  moveto (xf/4) ((2*yf/3)-50);
  draw_string "(_)        (_)  (_)  (_)         (_)(_)   (_) _    _(_)(_)(_)(_)  (_)         (_)(_)";
  moveto (xf/4) ((2*yf/3)-60);
  draw_string "(_)_  _  _ (_)_ (_) _(_) _  _  _ (_)(_)      (_) _(_)_  _  _ (_)_ (_)         (_)(_) _ _  _  _";
  moveto (xf/4) ((2*yf/3)-70);
  draw_string "(_)(_)(_)(_)  (_)(_)(_)  (_)(_)(_)   (_)         (_) (_)(_)(_)  (_)(_)         (_)(_)(_)(_)(_)(_)";

  set_color green;
  draw_rect (xf/4) (yf/4) (xf/2) (yf/7);
  fill_rect (xf/4) (yf/4) (xf/2) (yf/7);
  set_color black; moveto ((xf/4)+((xf/2)/2)-20) ((yf/4)+(yf/7)/2); draw_string "Play Now!";

  set_color red;
  draw_rect (xf/4) ((yf/4)+(yf/7)+20) (xf/2) (yf/7);
  fill_rect (xf/4) ((yf/4)+(yf/7)+20) (xf/2) (yf/7);
  set_color black; moveto ((xf/4)+((xf/2)/2)-20) (((yf/4)+(yf/7)+20) + (yf/7)/2); draw_string "Quit Game";

  let starting =
    wait_next_event [Button_down] in
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  if (*if green*)(starting.mouse_x >= xf/4 && starting.mouse_x<= ((xf/4) + (xf/2))) && (starting.mouse_y >= yf/4 && starting.mouse_y<= ((yf/4) + (yf/7)))
  then loop ()
  else if (*red*)(starting.mouse_x >= xf/4 && starting.mouse_x<= ((xf/4) + (xf/2))) && (starting.mouse_y >= ((yf/4)+(yf/7)+20) && starting.mouse_y<= (((yf/4)+(yf/7)+20) + (yf/7)))
  then ()
  else (*no box clicked*) opening ()





(* let starting = wait_next_event [Button_down] in
   if (starting.mouse_x >= (xf/4) && starting.mouse_x <= (xf/2)) &&
   (starting.mouse_y >= (yf/4) && starting.mouse_y <= (yf/7))
   then loop () else main () *)

(* let starting = wait_next_event [Key_pressed] in
   if starting.key == 's' then loop () else ()
   if (starting.mouse_x >= (xf/4) && starting.mouse_x <= (xf/2)) &&
   (starting.mouse_y >= (yf/4) && starting.mouse_y <= (yf/7))*)

(* moveto 200 200; Graphics.set_text_size 54; let (x,y)= Graphics.text_size "sdfsdfs" in draw_string (string_of_int x) *)


(***********)
