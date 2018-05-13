open Graphics
open Printf
open State
open Player
open Board
open Command
open Tile

type storage= {
  mutable message: string;
}

let stor = {
  message = ""
}


type gamescreen = {
  mutable state: state; (*state.init_state 10*)
  mutable p1messages: string;
  mutable p2messages: string;
  gwindow: int*int;
  gboard: int*int*int*int; (*200, 175 ,400, 400*)

  gp1buttons: (int*int*int*int) list;
  gp1rti: int *int *int *int;
  gp1fx: int* int* int* int;
  gp1fy: int*int*int*int;
  gp1rot: int*int*int*int;
  mutable gp1inv: (Tile.tile_id * (int*int*int*int)) list;
  (* mutable canvas1: ((int*int)*color) list; *)
  mutable canvas1tile: Tile.tile_id option;

  gp2buttons: (int*int*int*int) list;
  gp2rti: int *int *int *int;
  gp2fx: int* int* int* int;
  gp2fy: int*int*int*int;
  gp2rot: int*int*int*int;
  mutable gp2inv: (Tile.tile_id * (int*int*int*int)) list;
  (* mutable canvas2: ((int*int)*color) list; *)

  gregions: (int*int*int*int) list
}

let game = {
  state = State.init_state 10;
  gwindow = (1200, 750);
  gboard = (200, 175, 400, 400);
  p1messages = "";
  p2messages = "";
  gp1buttons = [(200,274,390,340); (200,208,295,274); (295,208,390,274); (200,142,390,208)];
  gp1rti= 200,274,190,66;
  gp1fx= 200,208,95,66;
  gp1fy= 295,208,95,66;
  gp1rot = 200,142,190,66;
  gp1inv = [(One, (30, 680, 30, 30)); (Tee, (120, 620, 90, 90));
            (L, (270, 620, 90, 90)); (X, (30, 500, 90, 90));
            (Z, (150, 500, 90, 90)); (Tree, (270, 500, 90, 90)); (Line, (150, 410, 60, 30))];
  (* canvas1 = [((-1,1),White);  ((0,1),White);  ((1,1),White);
                ((-1,0),White);  ((0,0),White);  ((1,0),White);
             ((-1,-1),White); ((0,-1),White); ((1,-1),White)]; *)
  canvas1tile= None;

  gp2buttons = [(1000,274,190,66); (1000,208,95,66); (1095,208,95,66); (1000,142,190,66)];
  gp2rti= 1000,274,190,66;
  gp2fx= 1000,208,95,66;
  gp2fy= 1095,208,95,66;
  gp2rot= 1000,142,190,66;
  gp2inv = [(One, (830, 680, 30, 30)); (Tee, (920, 680, 90, 90));
            (L, (1070, 680, 90, 90)); (X, (830, 500, 90, 90));
            (Z, (950, 500, 90, 90)); (Tree, (1070, 500, 90, 90)); (Line, (950, 410, 60, 30))];
  (* canvas2 = [((-1,1),Blue);  ((0,1),Blue);  ((1,1),Blue);
               ((-1,0),Blue);  ((0,0),Blue);  ((1,0),Blue);
             ((-1,-1),Blue); ((0,-1),Blue); ((1,-1),Blue)]; *)

  gregions = [(10,390,350,740); (200,390,142,400); (400, 760, 175, 575)]
}

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
        | Empty -> draw_tiles_helper t i;
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

 let rec tiles_searcher inv name player_id=
  match inv with
  | [] -> None
  | h::t -> if h.name = name
    then Some h
    else tiles_searcher t name player_id

let rec draw_onto_canvas_helper canvas player_id=
  match canvas with
  | [] -> set_color black;
  | ((x,y), color)::t ->
    begin
      let fill_color =
      (match color with
        | White -> white
        | Blue -> blue
        | Yellow -> yellow) in
      let pt1= 10 + (60* (x+1)) + (800*(player_id)) in
      let pt2= 142 + ((200/3) * (y+1)) in
      (draw_gui_rect pt1 pt2 60 67 fill_color;
       draw_onto_canvas_helper t player_id;)
    end

let draw_onto_canvas tile_name player_id=
  let inv= ((Array.of_list(game.state.players)).(player_id)).remaining_tiles in
  let tile= tiles_searcher inv tile_name player_id in
  match tile with
  | None -> set_color black;
  | Some x ->
    let tilecells= x.grid in
    (* if player_id=0
    then (game.canvas1 <- tilecells; game.canvas1tile <- Some tile_name)
    else game.canvas2 <- tilecells; *)
    if player_id=0 then (game.state.canvas1 <- tilecells; game.canvas1tile <- Some tile_name )
       else (game.state.canvas2 <- tilecells ; game.canvas1tile <- Some tile_name);
    draw_onto_canvas_helper tilecells player_id


(* let rec transform_tile tile_name *)
let rec click_inventory lst px py player_id=
  match lst with
  | [] -> set_color black
  | (n, (x,y,w,h))::t ->
    if (px>=x && px<=(x+w)) && (py>=y && py<=(y+h))
    then
      (
        (* stor.message <- "LOOKS GOOD!!!"; *)
       draw_onto_canvas n player_id)
      else
        click_inventory t px py player_id

let rec click_buttons px py player_id=
  (* match lst with
  | [] -> set_color black;
  | (x,y,w,h)::t -> *)
    (match game.canvas1tile with
    | None -> set_color black;
    | Some x ->
      if (px>=200 && px<=390) && (py>=274 && py<=340) then
        game.state <- (do_command (TURN x) game.state)
        (* stor.message <- "You Clicked Turn.!" *)
      else if (px>200 && px<=295) && (py>208 && py<274) then
  game.state <- (do_command (FLIPX x) game.state)
  (* stor.message <- "You Clicked FLIP X.!" *)
      else if (px>=295 && px<390) && (py>208 && py<274) then
  game.state <- (do_command (FLIPY x) game.state)
  (* stor.message <- "You Clicked FLIPY.!" *)
      else if (px>200 && px<390) && (py>=142 && py<208) then
  game.state <- (do_command (FORFEIT) game.state)
  (* stor.message <- "You Clicked FORFEIT.!" *)
      else
        click_buttons px py player_id)


                  (*******************************)
let rec loop () =

  clear_graph ();
  draw_string stor.message;
  draw_tiles game.state.players;
  set_color black;

  (*Board setup*)
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  let xboard = 400 in
  let yboard = 400 in
  let xboardleftcorner= 400 in

  (*Board cell set up*)
  for x = 0 to 9
  do
    for y = 0 to 9 do
      let pt1 = 400 + (40 * (x)) in
      let pt2 = 575 - (40 * (y+1)) in
      draw_rect pt1 pt2 40 40;
      moveto pt1 pt2; draw_string ("("^(string_of_int x)^", "^(string_of_int y)^")");
    done;
  done;

  (*Player Inventory set up*)
  (*Player 1 Inventory*)
  draw_rect 10 350 380 390;
  moveto 150 720; draw_string ("Player 1 Inventory");
  (*Player 2 Inventory*)
  draw_rect (xboardleftcorner + xboard + 10) 350 380 390;
  moveto 950 720; draw_string ("Player 2 Inventory");
  (*Player Canvas set up*)
  (*Player 1 Canvas*)
  (* for x = -1 to 1 do
    for y = -1 to 1 do
      let pt1 = 10 + ((((xf-xboard)/4 - 20)/3) * (x+1)) in
      let pt2 = (yf- 610) + ((200/3) * (y+1)) +2 in
      draw_rect pt1 pt2 (((xf-xboard)/4 - 20)/3) (200/3);
      moveto pt1 pt2; draw_string ("("^(string_of_int (x))^", "^(string_of_int (y))^")");
    done;
  done; *)

  (*Player 1 Buttons*)
  draw_gui_button game.gp1rti black;
  draw_gui_text 243 304 "Rotate Tile" black;

  draw_gui_button game.gp1fx black;
  draw_gui_text 230 238 "Flip X" black;

  draw_gui_button game.gp1fy black;
  draw_gui_text 325 238 "Flip Y" black;

  draw_gui_button game.gp1rot black;
  draw_gui_text 248 172 "Forfeit" black;

  (*Player 1 Message Board*)
  draw_rect 10 10 380 120;

  (*Player 2 Buttons*)
  draw_gui_button game.gp2rti black;
  draw_gui_text 1043 304 "Return to Inventory" black;

  draw_gui_button game.gp2fx black;
  draw_gui_text 1030 238 "Flip X" black;

  draw_gui_button game.gp2fy black;
  draw_gui_text 1125 238 "Flip Y" black;

  draw_gui_button game.gp2rot black;
  draw_gui_text 1048 172 "Rotate Clockwise" black;

  (*Player 2 Message Board*)
  draw_rect (xboard + (xf/3)+10 ) 10 ((xf-xboard)/2 - 20) 120;

  (*Messaage Board*)
  draw_rect xboardleftcorner 10 xboard ((yf-yboard)/2 -20);

  (***********)

  draw_onto_canvas_helper game.state.canvas1 0;
  draw_onto_canvas_helper game.state.canvas2 1;
  set_color black;


  (* let draw_button fig =
    match fig with
    | (s,c, (x,y,w,h)) -> set_color c; fill_rect x y w h; set_color black

  in *)

  (*Making a button*)
  (* let lst= [("REDS", red, (200,200,150,150)); ("BLUES", blue, (400,200,150,150))] in
  for x= 0 to 1 do
    draw_button (List.nth lst x)
  done; *)

  let click () =
    let clicker = Graphics.wait_next_event [Button_down] in
    (clicker.mouse_x, clicker.mouse_y) in
  let (px,py)= click () in
  let rec which_button regions=
    match regions with
    | [] -> stor.message <- stor.message ;
    | ((x1,x2,y1,y2))::t ->
      if ((px>=10 && px<=390) && (py>=350 && py<=740))
      then
        begin
          (* stor.message <- "You Clicked P1 Inv Reg.!"; *)
          let lst= game.gp1inv in
          (click_inventory lst px py 0)
        end
      else if ((px>=200 && px<=390) && (py>= 142 && py<=406))
      then
        begin
          (* stor.message <- "You Clicked P1 Buttons Reg.!"; *)
          (* let lst = game.gp1buttons in *)
          (click_buttons px py 0) end

      (* else if ((px>=10 && px<=390) && (py>=350 && py<=740))
      then stor.message <- "You Clicked P2 Inv Reg.!"

      else if ((px>=10 && px<=390) && (py>=350 && py<=740))
         then stor.message <- "You Clicked P2 Button Reg.!" *)
          (* (400, 760, 175, 535) *)
      else if ((px>=400 && px<=760) && (py>=175 && py<=575))
      then
        (* stor.message <- "You Clicked BOARD.!" *)
        match game.canvas1tile with
        | None -> stor.message <- "NOPE!"
        | Some x ->
          begin
            game.state <- do_command (PLACE ((px,py),x)) game.state;
            game.canvas1tile <- None
          end
      else
        which_button t;

  in
  begin
    moveto 200 200; which_button game.gregions; clear_graph(); loop ();
  end

  (* moveto 150 150; click_mapper (clicker ()); clear_graph(); loop (); *)
(*
  let clicker () =
    let cl= Graphics.wait_next_event [Button_down] in
    cl.mouse_x , cl.mouse_y
  in
    let (px , py)= clicker () in
      let val = if (px>=10 && px<=380) && (py>=350 && py<=390)
      then stor.message <- "You Clicked on inv!" in

    begin moveto 0 200; clicker (); clear_graph(); loop (); end  *)


  (* (*
     let click () =
      let clicker = Graphics.wait_next_event [Button_down] in
      (clicker.mouse_x, clicker.mouse_y) in
     let (px,py)= click () in
     let rec which_button lst=
     match lst with
     | [] -> stor.message <- stor.message;
     | (s,c,(x,y,w,h))::t -> if (px>=x && px<=(x+w)) && (py>=y && py<=(y+h))
      then
        begin
           stor.message <- "You Clicked on "^s^"!";
        end
      else
        which_button t;

     in
     begin
     moveto 0 200; which_button lst; clear_graph(); loop ();
     end *) *)
  (* let e = wait_next_event [Mouse_motion; Key_pressed] in

     let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in
     let key_description = if e.keypressed then sprintf "Key %c was pressed" e.key else "" in


     clear_graph ();
     moveto 0 100; draw_string key_description;
     moveto 0 0; draw_string mouse_description; *)

  (* let draw_button fig =
    match fig with
    | (s,c, (x,y,w,h)) -> set_color c; fill_rect x y w h; set_color black

  in

  (*Making a button*)
  let lst= [("REDS", red, (200,200,150,150)); ("BLUES", blue, (400,200,150,150))] in
  for x= 0 to 1 do
    draw_button (List.nth lst x)
  done; *)

  (* let clicker () =
    let cl = Graphics.wait_next_event [Button_down] in
    (cl.mouse_x, cl.mouse_y) in
  let (px,py)= clicker () in

  if (px>=10 && px<=380) && (py>=350 && py<=390)
  then
    let lst= game.gp1inv in
    click_p1_inventory lst pt 0
    (* else if (px>=10 && px<=380) && (py>=350 && py<=390)
       then
       let lst= game.gp1buttons in
       click_p1_buttons lst pt *)
  else (); *)


  (***********)





(* returns the relative (x, y) of the next mouse click within the
 * grect defined by (x, y, w, h), doesn't terminate until the mouse is clicked
 * within the bounds of the window. *)




(* let starting = wait_next_event [Key_pressed] in
if starting.key == 's' then loop () else () *)


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
