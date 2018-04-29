open Tile
open Player
open State
open Command

open Graphics
open Printf
open Gui


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
  draw_rect xboardleftcorner yboardleftcorner xboard yboard;

  (*Board cell set up*)
  for x = 1 to 10
  do
    for y = 1 to 10 do
      let pt1 = xboardleftcorner + (yboard/10 * (x-1)) in
      let pt2 = yboardleftcorner + (xboard/10 * (y-1)) in
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
  (* draw_rect 10 (yf- 610) ((xf-xboard)/4 - 20) 200; *)
  for x = 1 to 3 do
    for y = 1 to 3 do
      let pt1 = 10 + ((((xf-xboard)/4 - 20)/3) * (x-1)) in
      let pt2 = (yf- 610) + ((200/3) * (y-1)) +2 in
      draw_rect pt1 pt2 (((xf-xboard)/4 - 20)/3) (200/3);
      moveto pt1 pt2; draw_string ("P1("^(string_of_int x)^", "^(string_of_int y)^")");
    done;
  done;

  (*Player 1 Buttons*)
  (* set_color blue; fill_rect ((xf-xboard)/4) (yf- 476) (((xf-xboard)/4) -10) 66; *)
  draw_rect ((xf-xboard)/4) (yf- 476) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto (((xf-xboard)/4)+43) (yf- 476 +30); draw_string ("Return to Inventory");

  (* set_color red; fill_rect ((xf-xboard)/4) (yf- 542) (((xf-xboard)/4) -10) 66; *)
  draw_rect ((xf-xboard)/4) (yf- 542) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto (((xf-xboard)/4)+83) (yf- 542 +30 ); draw_string ("Flip");


  (* set_color yellow; fill_rect ((xf-xboard)/4) (yf- 608) (((xf-xboard)/4) -10) 66; *)
  draw_rect ((xf-xboard)/4) (yf- 608) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto (((xf-xboard)/4)+48) (yf- 608 +30); draw_string ("Rotate Clockwise");

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

  (*Player 2 Buttons*)
  (* set_color blue; fill_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 476) (((xf-xboard)/4) -10) 66; *)
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 476) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto ((xboard + (xf/3)+((xf-xboard)/4))+43) (yf- 476 +30); draw_string ("Return to Inventory");

  (* set_color yellow; fill_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 542) (((xf-xboard)/4) -10) 66; *)
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 542) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto ((xboard + (xf/3)+((xf-xboard)/4))+83) (yf- 542 +30 ); draw_string ("Flip");

  (* set_color red; fill_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 608) (((xf-xboard)/4) -10) 66; *)
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 608) (((xf-xboard)/4) -10) 66;
  set_color black;
  moveto ((xboard + (xf/3)+((xf-xboard)/4))+48) (yf- 608 +30); draw_string ("Rotate Clockwise");

  draw_rect (xboard + (xf/3)+10 ) 10 ((xf-xboard)/2 - 20) 120;

  (*Messaage Board*)
  draw_rect xboardleftcorner 10 xboard ((yf-yboard)/2 -20);


  let starting = wait_next_event [Key_pressed] in
  if starting.key == 's' then loop () else ()



  (* let e = wait_next_event [Mouse_motion] in
  let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in
  clear_graph ();
  moveto 0 0; draw_string mouse_description;
  if e.key <> 'q' then loop () else () *)


let rec main ()=
  clear_graph ();
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  moveto (xf/2) (2*yf/3);
  draw_string "bLoKaML";
  set_color green;
  draw_rect (xf/4) (yf/4) (xf/2) (yf/7);
  set_color red;
  draw_rect (xf/4) ((yf/4)+(yf/7)+20) (xf/2) (yf/7);

  let starting =
    wait_next_event [Button_down] in
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  if (*if green*)(starting.mouse_x >= xf/4 && starting.mouse_x<= ((xf/4) + (xf/2))) && (starting.mouse_y >= yf/4 && starting.mouse_y<= ((yf/4) + (yf/7)))
  then loop ()
  else if (*red*)(starting.mouse_x >= xf/4 && starting.mouse_x<= ((xf/4) + (xf/2))) && (starting.mouse_y >= ((yf/4)+(yf/7)+20) && starting.mouse_y<= (((yf/4)+(yf/7)+20) + (yf/7)))
  then ()
  else (*no box clicked*) main()

(* let starting = wait_next_event [Button_down] in
   if (starting.mouse_x >= (xf/4) && starting.mouse_x <= (xf/2)) &&
   (starting.mouse_y >= (yf/4) && starting.mouse_y <= (yf/7))
   then loop () else main () *)

(* let starting = wait_next_event [Key_pressed] in
   if starting.key == 's' then loop () else ()
   if (starting.mouse_x >= (xf/4) && starting.mouse_x <= (xf/2)) &&
   (starting.mouse_y >= (yf/4) && starting.mouse_y <= (yf/7))*)

(* moveto 200 200; Graphics.set_text_size 54; let (x,y)= Graphics.text_size "sdfsdfs" in draw_string (string_of_int x) *)



let () =
  open_graph " 1200x750";
  Graphics.set_window_title "bLoKaML by Srishti B., Kati H., Sahithi K., & Devki T.";
  main ();
  (* loop (); *)
  close_graph ();

  (* brew install Caskroom/cask/xquartz
     brew reinstall ocaml --with-x11 *)
