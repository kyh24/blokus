open Tile
open Player
open State
open Command

open Graphics
open Printf
open Gui


let rec loop () =
  let e = wait_next_event [Mouse_motion; Key_pressed] in

  let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in


  clear_graph ();
  moveto 0 0; draw_string mouse_description;

  if e.key <> 'q' then loop () else ()


let rec main ()=
  clear_graph ();
  let x= Graphics.size_x () in
  let y= Graphics.size_y () in
  moveto (x/2) (2*y/3);
  draw_string "bLoKaML";
  set_color red;
  draw_rect (x/4) (y/4) (x/2) 100;

  let starting = wait_next_event [Button_up ] in
  if (starting.button = true)
  &&(starting.mouse_x >= (x/4) && starting.mouse_x <= (x/2)) &&
     (starting.mouse_y >= (x/4) && starting.mouse_y <= 100)
  then loop () else ()
(*
  let starting = wait_next_event [Key_pressed] in
  if starting.key != 's' then main () else main () *)


  (* moveto 200 200; Graphics.set_text_size 54; let (x,y)= Graphics.text_size "sdfsdfs" in draw_string (string_of_int x) *)



let () =
  open_graph " 700x700";
  Graphics.set_window_title "bLoKaML by Srishti B., Kati H., Sahithi K., & Devki T.";
  main ();
  loop ();
  close_graph ();

(* brew install Caskroom/cask/xquartz
brew reinstall ocaml --with-x11 *)
