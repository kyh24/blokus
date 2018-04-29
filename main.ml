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
  for x = 0 to 7
  do
    for y = 0 to 7 do
      let pt1 = xboardleftcorner + (yboard/8 * x) in
      let pt2 = yboardleftcorner + (xboard/8 * y) in
      draw_rect pt1 pt2 (xboard/8) (yboard/8);
    done;
  done;

  (*Player Inventory set up*)
  (*Player 1 Inventory*)
  draw_rect 10 (yf- 400) ((xf-xboard)/2 - 20) 390;
  (*Player 2 Inventory*)
  draw_rect (xboardleftcorner + xboard + 10) (yf- 400) ((xf-xboard)/2 - 20) 390;

  (*Player Canvas + buttons set up*)
  (*Player 1 Canvas*)
  draw_rect 10 (yf- 610) ((xf-xboard)/4 - 20) 200;

  draw_rect ((xf-xboard)/4) (yf- 476) (((xf-xboard)/4) -10) 66;
  draw_rect ((xf-xboard)/4) (yf- 542) (((xf-xboard)/4) -10) 66;
  draw_rect ((xf-xboard)/4) (yf- 608) (((xf-xboard)/4) -10) 66;
  draw_rect 10 10 ((xf-xboard)/2 - 20) 120;
  (*Player 2 Canvas (810) *)
  draw_rect (xboard + (xf/3)+10 ) (yf- 610) ((xf-xboard)/4 - 20) 200;
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 476) (((xf-xboard)/4) -10) 66;
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 542) (((xf-xboard)/4) -10) 66;
  draw_rect (xboard + (xf/3)+((xf-xboard)/4)) (yf- 608) (((xf-xboard)/4) -10) 66;
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
  set_color red;
  draw_rect (xf/4) (yf/4) (xf/2) (yf/7);

  let starting = wait_next_event [Key_pressed] in
  if starting.key == 's' then loop () else ()

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
