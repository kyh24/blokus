open Graphics
open Printf
open State
open Player
open Board
open Command
open Tile

(**** FOR TESTING ERROR MESSAGES IN GUI: WILL REMOVE! ****)
type storage= {
  mutable message: string;
}
let stor = {
  message = ""
}

(**** GUI STATE ****)

(* gamescreen contains state as well as other fields that are useful
   in developing the GUI window.*)
type gamescreen = {
  mutable state: state;
  mutable p1messages: string;
  mutable p2messages: string;
  gregions: (int*int*int*int) list;
  mutable gwinner : string;

  gp1buttons: (int*int*int*int) list;
  gp1rti: int *int *int *int;
  gp1fx: int* int* int* int;
  gp1fy: int*int*int*int;
  gp1rot: int*int*int*int;
  mutable gp1inv: (Tile.tile_id * (int*int*int*int)) list;
  mutable canvas1tile: Tile.tile_id option;

  gp2buttons: (int*int*int*int) list;
  gp2rti: int *int *int *int;
  gp2fx: int* int* int* int;
  gp2fy: int*int*int*int;
  gp2rot: int*int*int*int;
  mutable gp2inv: (Tile.tile_id * (int*int*int*int)) list;
  mutable canvas2tile: Tile.tile_id option
}


(* game is of type gamescreen and contains state as well as other
   fields that are useful in developing the GUI window.  Some of these
   fields will be updated as users change the state of the game or modify
   the window. *)
let game = {
  state = State.init_state 16;
  p1messages = "Please select a tile.";
  p2messages = "";
  gregions = [(10,390,350,740); (200,390,142,400);
              (810, 1190, 350, 740); (400, 760, 175, 575)];
  gwinner= "";

  gp1buttons = [(200,274,390,340); (200,208,295,274);
                (295,208,390,274); (200,142,390,208)];
  gp1rti= 200,274,190,66;
  gp1fx= 200,208,95,66;
  gp1fy= 295,208,95,66;
  gp1rot = 200,142,190,66;
  gp1inv = [(One, (20, 680, 20, 20)); (Tee, (70, 640, 60, 60));
            (L, (150, 640, 60, 60)); (X, (240, 640, 60, 60));
            (Z, (320, 640, 60, 60)); (Tree, (20, 520, 60, 60));
            (Line, (100, 520, 20, 60)); (Two, (140,540,40,20));
            (Four, (200,520,40,40)); (Boot,(260,520,40,60));
            (Kink, (320,520,60, 40)); (C,(20,400,40,60));
            (Cowgirl, (80,400,60,40)); (Couch, (160,400,60,40));
            (Recliner, (240,400,40,40)); (Stairmaster, (300,400,60,60))];
  canvas1tile= None;

  gp2buttons = [(1000,274,190,66); (1000,208,95,66);
                (1095,208,95,66); (1000,142,190,66)];
  gp2rti= 1000,274,190,66;
  gp2fx= 1000,208,95,66;
  gp2fy= 1095,208,95,66;
  gp2rot= 1000,142,190,66;
  gp2inv = [(One, (820, 680, 20, 20)); (Tee, (870, 640, 60, 60));
            (L, (950, 640, 60, 60)); (X, (1040, 640, 60, 60));
            (Z, (1120, 640, 60, 60)); (Tree, (820, 520, 60, 60));
            (Line, (900, 520, 20, 60)); (Two, (940,540,40,20));
            (Four, (1000,520,40,40)); (Boot,(1060,520,40,60));
            (Kink, (1120,520,60, 40)); (C,(820,400,40,60));
            (Cowgirl, (880,400,60,40)); (Couch, (960,400,60,40));
            (Recliner, (1040,400,40,40)); (Stairmaster, (1100,400,60,60))];
  canvas2tile = None
}

(**** HELPER GUI MOUSE CLICK AND DRAWING FUNCTIONS ****)
(* [draw_gui_rect] draws filled rectangle on the GUI window
   when given a starting point (x and y), the width w, the height h, and
   the color of the filled shape. Intended for drawing cells. *)

(* let blue = Graphics.rgb 95 173 225 *)
let blue = Graphics.rgb 52 142 229
let yellow= Graphics.rgb 250 222 39
let red= Graphics.rgb 215 10 26
let green= Graphics.rgb 104 218 56
let gray= Graphics.rgb 94 94 94

let draw_gui_rect x y w h color =
  set_color color;
  fill_rect x y w h;
  set_color black

(* [draw_gui_button figure color] draws empty rectangle outlines on the GUI window
   when given a starting point (x and y), the width w, the height h, and
   the color of the outline. Intended for drawing GUI buttons.*)
let draw_gui_button figure color=
  match figure with
  |(x,y,w,h) -> set_color color; draw_rect x y w h;
  set_color black

(* [draw_gui_text x y str color] draws a string on the GUI window
   when given a starting point (x and y), the string str, and
   the color of the text. *)
let draw_gui_text x y str color =
  set_color color;
  moveto x y;
  draw_string (str);
  set_color black

(**** HELPER DRAW TILE FUNCTIONS ****)
(* [draw_tiles_helper tilelist i] draws the entire inventory of a player in their
   respective inventory board.  Takes in [tilelist] which is the list
   of tiles in the player's remaining_tiles list.  Takes in [i] which is
   the index of the player.  *)
let rec draw_tiles_helper tilelist i=
  match tilelist with
  | [] -> set_color black;
  | h::t ->
    if i=0 then set_color yellow else set_color blue;
    begin
        match h.name with
        | One ->  fill_rect (20 +(800*i)) 680 20 20; draw_tiles_helper t i;
        | Tee ->  fill_rect (70+(800*i)) 680 20 20;
                  fill_rect (90+(800*i)) 680 20 20;
                  fill_rect (110+(800*i)) 680 20 20;
                  fill_rect (90+(800*i)) 660 20 20;
                  fill_rect (90+(800*i)) 640 20 20; draw_tiles_helper t i;
        | L ->    fill_rect (150 +(800*i)) 680 20 20;
                  fill_rect (150 +(800*i)) 660 20 20;
                  fill_rect (150 +(800*i)) 640 20 20;
                  fill_rect (170 +(800*i)) 640 20 20;
                  fill_rect (190 +(800*i)) 640 20 20; draw_tiles_helper t i;
        | X ->    fill_rect (260 +(800*i)) 680 20 20;
                  fill_rect (260 +(800*i)) 660 20 20;
                  fill_rect (260 +(800*i)) 640 20 20;
                  fill_rect (240 +(800*i)) 660 20 20;
                  fill_rect (280 +(800*i)) 660 20 20; draw_tiles_helper t i;
        | Z ->    fill_rect (320+(800*i))  680 20 20;
                  fill_rect (340+(800*i))  680 20 20;
                  fill_rect (340+(800*i))  660 20 20;
                  fill_rect (340+(800*i))  640 20 20;
                  fill_rect (360+(800*i))  640 20 20; draw_tiles_helper t i;
        | Tree -> fill_rect (20+(800*i)) 560 20 20;
                  fill_rect (40+(800*i)) 560 20 20;
                  fill_rect (40+(800*i)) 540 20 20;
                  fill_rect (40+(800*i)) 520 20 20;
                  fill_rect (60+(800*i)) 540 20 20; draw_tiles_helper t i;
        | Line -> fill_rect (100+(800*i)) 560 20 20;
                  fill_rect (100+(800*i)) 540 20 20;
                  fill_rect (100+(800*i)) 520 20 20; draw_tiles_helper t i;
        | Two ->  fill_rect (140+(800*i)) 540 20 20;
                  fill_rect (160+(800*i)) 540 20 20; draw_tiles_helper t i;
        | Four -> fill_rect (200+(800*i)) 540 20 20;
                  fill_rect (220+(800*i)) 540 20 20;
                  fill_rect (200+(800*i)) 520 20 20;
                  fill_rect (220+(800*i)) 520 20 20; draw_tiles_helper t i;
        | Boot -> fill_rect (260+(800*i)) 560 20 20;
                  fill_rect (260+(800*i)) 540 20 20;
                  fill_rect (260+(800*i)) 520 20 20;
                  fill_rect (280+(800*i)) 540 20 20;
                  fill_rect (280+(800*i)) 520 20 20; draw_tiles_helper t i;
        | Kink -> fill_rect (320+(800*i)) 520 20 20;
                  fill_rect (340+(800*i)) 520 20 20;
                  fill_rect (340+(800*i)) 540 20 20;
                  fill_rect (360+(800*i)) 540 20 20; draw_tiles_helper t i;
        | C ->    fill_rect (20+(800*i)) 440 20 20;
                  fill_rect (40+(800*i)) 440 20 20;
                  fill_rect (20+(800*i)) 420 20 20;
                  fill_rect (20+(800*i)) 400 20 20;
                  fill_rect (40+(800*i)) 400 20 20; draw_tiles_helper t i;
        | Cowgirl ->
                  fill_rect (100+(800*i)) 420 20 20;
                  fill_rect (80+(800*i)) 400 20 20;
                  fill_rect (100+(800*i)) 400 20 20;
                  fill_rect (120+(800*i)) 400 20 20; draw_tiles_helper t i;
        | Couch ->
                  fill_rect (200+(800*i)) 420 20 20;
                  fill_rect (160+(800*i)) 400 20 20;
                  fill_rect (180+(800*i)) 400 20 20;
                  fill_rect (200+(800*i)) 400 20 20; draw_tiles_helper t i;
        | Recliner ->
                  fill_rect (240+(800*i)) 420 20 20;
                  fill_rect (240+(800*i)) 400 20 20;
                  fill_rect (260+(800*i)) 400 20 20; draw_tiles_helper t i;
        | Stairmaster->
                  fill_rect (340+(800*i)) 440 20 20;
                  fill_rect (320+(800*i)) 440 20 20;
                  fill_rect (320+(800*i)) 420 20 20;
                  fill_rect (300+(800*i)) 420 20 20;
                  fill_rect (300+(800*i)) 400 20 20; draw_tiles_helper t i;

      end

(* [draw_tiles playerlist] determines the right inventory set being searched through
   and changed with the tile selection.  Takes in [playerlist] which is
   the list of players in this game.  *)
  let rec draw_tiles playerlist=
    let playerarray= Array.of_list playerlist in
    for i=0 to 1
    do
      let tilelist= (playerarray.(i)).remaining_tiles in
      draw_tiles_helper tilelist i
    done

(* [tiles_searcher inv name] searches the entire inventory of a player
   for a given tile name.  Takes in [inv] which is the list
   of tiles in the player's remaining_tiles list.  Takes in [name]
   which is name or tile_id of the tile selected.  If a tile with such
   tile_id exists, the option of that tile is returned otherwise None.*)
 let rec tiles_searcher inv name =
  match inv with
  | [] -> None
  | h::t -> if (h.name = name)
    then Some h
    else tiles_searcher t name

(* [draw_onto_canvas_helper canvas player_id] draws teh canvas of the player specified.
   Takes [canvas] to draw the cells of the canvas in a loop.  Takes
   [player_id] to calculate placement of the canvas on the GUI window
   according to player id.*)
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

(* [draw_onto_canvas tile_name player_id] draws the tile on the right player's canvas.
   Takes in [tile_name] which is the name of the tile selected.
   Takes in [player_id] which is the index of the player.
    Changes the tile on the canvas, changes the player message if needed,
    and will use [draw_onto_canvas_helper] to draw in tile on respective
    canvas.*)
let draw_onto_canvas tile_name player_id=
  let inv= ((Array.of_list(game.state.players)).(player_id)).remaining_tiles in
  let tile= tiles_searcher inv tile_name in
  match tile with
  | None ->
    if player_id=0
    then game.p1messages <- "Please select a tile."
    else if player_id=1
    then game.p2messages <- "Please select a tile."
    else (game.p1messages <- game.p1messages ;
          game.p2messages <- game.p2messages)
  | Some x ->
    let tilecells= x.grid in
    if player_id=0
    then (game.state.canvas1 <- tilecells;
          game.canvas1tile <- Some x.name;
          game.p1messages <- "Modify & place tile.")
    else (game.state.canvas2 <- tilecells ;
          game.canvas2tile <- Some x.name;
          game.p2messages <- "Modify & place tile.");
    draw_onto_canvas_helper tilecells player_id


(* [draw_onto_board lst] draws the board recursively depending on which tiles
   have been clicked and place Takes in [lst] which is the same thing as board,
   but changed to list using Array.tolist for the purpose of pattern matching
   easily.*)
let rec draw_onto_board lst=
  match lst with
  | [] -> set_color black;
  | ((x,y), color)::t ->
    begin
      let fill_color =
        (match color with
         | White -> white
         | Blue -> blue
         | Yellow -> yellow)
      in
      let pt1 = 400 + (25 * (x)) in
      let pt2 = 575 - (25 * (y+1)) in
      draw_gui_rect pt1 pt2 25 25 fill_color;

      begin
        if (x=0 && y=0) then (set_color yellow; fill_ellipse (pt1+12) (12+pt2) 8 8)
        else if (x=15 && y=15) then (set_color blue; fill_ellipse (pt1+12) (12+pt2) 8 8)
      end;
      set_color black;
      draw_rect pt1 pt2 25 25;
      (* moveto pt1 pt2;
      draw_string ("("^(string_of_int x)^", "^(string_of_int y)^")"); *)
      (draw_onto_board t);

    end

(* [click_inventory lst px py player_id] links the click within the inventory box to a tile
   and starts the drawing onto canvas process if a tile was clicked in the
   GUI.  Takes in [lst] which is the gui inventory list.
   Takes in [px] and [py] which are the positions of the mouse clicks.
   Takes [player_id] to be used in helper function calculations and
   processing later.*)
let rec click_inventory lst px py player_id=
  match lst with
  | [] ->
    (* if player_id=0
    then game.p1messages <- "Please select a tile."
    else if player_id=1
    then game.p2messages <- "Please select a tile."; *)
    (* else (game.p1messages <- game.p1messages ;
          game.p2messages <- game.p2messages); *)
    game.canvas1tile <- game.canvas1tile;
    game.canvas2tile <- game.canvas2tile
  | (n, (x,y,w,h))::t ->
    if (px>=x && px<=(x+w)) && (py>=y && py<=(y+h))
    then
      (
        (* (if (player_id =0 )
        then game.canvas1tile <- Some n
        else if (player_id=1) then game.canvas2tile <-Some n); *)
        (* stor.message <- "LOOKS GOOD!!!"; *)
       draw_onto_canvas n player_id)
      else
        click_inventory t px py player_id

(* [click_buttons_p1 px py] links the click within the player 1's box of tile
   transformation buttons to a specific transformation/command.
   Starts the drawing onto canvas process to show transformation selected.  '
   Takes in [px] and [py] which are the positions of the mouse clicks.*)
let rec click_buttons_p1 px py =
    (match game.canvas1tile with
    | None -> if (px>200 && px<390) && (py>=142 && py<208) then
        (game.p1messages <-  "No more future turns!";
         game.p2messages <-  "Please select a tile.";
           game.state <- (do_command (FORFEIT) game.state))
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
        (game.p1messages <-  "No more future turns!";
         game.p2messages <-  "Please select a tile.";
         game.state <- (do_command (FORFEIT) game.state))
  (* stor.message <- "You Clicked FORFEIT.!" *)
      else
        click_buttons_p1 px py )

(* [click_buttons_p2 px py] links the click within the player 2's box of tile
   transformation buttons to a specific transformation/command.
   Starts the drawing onto canvas process to show transformation selected.  '
   Takes in [px] and [py] which are the positions of the mouse clicks.*)
let rec click_buttons_p2 px py =
  (match game.canvas2tile with
   | None -> if (px>1000 && px<1190) && (py>=142 && py<208) then
       ( game.p2messages <-  "No more future turns!";
         game.p1messages <-  "Please select a tile.";
         game.state <- (do_command (FORFEIT) game.state)
       )
   | Some x ->
     if (px>=1000 && px<=1190) && (py>=274 && py<=340) then
       game.state <- (do_command (TURN x) game.state)
       (* stor.message <- "You Clicked Turn.!" *)
     else if (px>1000 && px<=1095) && (py>208 && py<274) then
       game.state <- (do_command (FLIPX x) game.state)
       (* stor.message <- "You Clicked FLIP X.!" *)
     else if (px>=1095 && px<1190) && (py>208 && py<274) then
       game.state <- (do_command (FLIPY x) game.state)
       (* stor.message <- "You Clicked FLIPY.!" *)
     else if (px>1000 && px<1190) && (py>=142 && py<208) then
       ( game.p2messages <-  "No more future turns!";
         game.p1messages <-  "Please select a tile.";
         game.state <- (do_command (FORFEIT) game.state)
       )
       (* stor.message <- "You Clicked FORFEIT.!" *)
     else
         (* "You Clicked error.!" ) *)
       click_buttons_p2 px py )

(* [getcurrentplayer st] finds the index of the current player in the state
   within the list of players.  Takes in [st] the state to access the
   player list.*)
let getcurrentplayer st =
  let playername= game.state.curr_player.player_name in
  if playername = "Player 1" then 0 else 1

(* [winner_detected st] will display the win message onto the message board
   if there is a winner. Takes in [st] the state to check if there is a win*)
let winner_detected st=
  if game.state.game_over = true then (game.gwinner <- (print_winner game.state))


let cover_options player=
  if (player=0 && game.state.game_over = false )then draw_gui_rect 1000 142 190 198 gray
  else if (player=1 && game.state.game_over = false ) then draw_gui_rect 200 142 190 198 gray
  else if game.state.game_over = true then (draw_gui_rect 1000 142 190 198 gray;draw_gui_rect 200 142 190 198 gray)


(* let place_tile_checker st px py=
  (let playerindex= getcurrentplayer game.state.curr_player in
       if (playerindex = 0) then
         begin
           match game.canvas1tile with
           | None ->
             Printf.printf "Hits NONE";
             game.p1messages <- "First select a tile!";
           | Some x ->
             begin
               let orig = ref game.state in
               let returnedst = (do_command (PLACE ((px,py),x)) !orig) in
               (if (returnedst != (game.state)) then
                  (Printf.printf "Hits Some, inval";
                   game.p1messages <- "Invalid Move - Try Again!";
                   game.p2messages <- ""
                  )
                else ( Printf.printf "Hits Some, command";
                       game.state <- returnedst;
                       game.p1messages <- "";
                       game.p2messages <- "Please select a tile.";
                       game.canvas1tile <- None;
                       game.canvas2tile <- None;

                     ))
             end
         end
       else if ( playerindex =1 ) then
         begin
           match game.canvas2tile with
           | None ->
             game.p2messages <- "First select a tile!"
           | Some y ->
             begin

               let orig = ref game.state in
               let returnedst = (do_command (PLACE ((px,py),y)) !orig) in
               (if (returnedst != (game.state)) then
                  (  Printf.printf "Hits Some, inval2";game.p1messages <- "";
                     game.p2messages <- "Invalid Move - Try Again!";
                     game.p1messages <- ""
                  )
                else ( Printf.printf "Hits Some, command2";
                       game.state <- returnedst;
                       game.p1messages <- "Please select a tile.";
                       game.p2messages <- "" ;
                       game.canvas1tile <- None;
                       game.canvas2tile <- None;

                     ))
             end
         end) *)

let place_tile_checker st px py=
  (let playerindex= getcurrentplayer st.curr_player in
   if (playerindex = 0) then
     begin
       match game.canvas1tile with
       | None ->
         (Printf.printf "Hits NONE1-";
         game.p1messages <- "First select a tile!";)
       | Some x ->
         begin
           let returnedst = (do_command (PLACE ((px,py),x)) {st with board=st.board}) in
           (if (returnedst = st) then
              (Printf.printf "Hits Some, inval1-";
               game.p1messages <- "Invalid Move - Try Again!";
               game.p2messages <- "";
               game.canvas1tile <- game.canvas1tile;
               game.canvas2tile <- game.canvas2tile;
              )
            else ( Printf.printf "Hits Some, command1-";
                   game.p1messages <- "";
                   game.p2messages <- "Please select a tile.";
                   game.canvas1tile <- None;
                   game.canvas2tile <- None;
                   game.state <- returnedst;
                 ))
         end
     end
   else if ( playerindex =1 ) then
     begin
       match game.canvas2tile with
       | None ->
        ( Printf.printf "Hits NONE2-";
         game.p2messages <- "First select a tile!")
       | Some y ->
         begin
           let returnedst = (do_command (PLACE ((px,py),y)) {st with board=st.board}) in
           (if (returnedst = st) then
              (  Printf.printf "Hits Some, inval2-";
                 game.p2messages <- "Invalid Move - Try Again!";
                 game.p1messages <- "";
                 game.canvas1tile <- game.canvas1tile;
                 game.canvas2tile <- game.canvas2tile;
              )
            else ( Printf.printf "Hits Some, command2-";
                   game.p1messages <- "Please select a tile.";
                   game.p2messages <- "" ;
                   game.canvas1tile <- None;
                   game.canvas2tile <- None;
                   game.state <- returnedst;

                 ))
         end
     end)
(*************************************************************************)

(* [loop ()] is the REPL that will display the game window and adapt with
    changes to state. *)
let rec loop () =
  clear_graph ();
  draw_gui_text 140 60 game.p1messages black;
  draw_gui_text 940 60 game.p2messages black;
  draw_string stor.message;
  winner_detected game.state;
  draw_tiles game.state.players;
  cover_options (getcurrentplayer game.state.curr_player);
  set_color black;


  (*Instructions*)
  moveto 580 720;
  draw_string ("BLOKAML");
  moveto 580 700;
  draw_string ("-Rules-");
  moveto 450 680;
  draw_string ("Goal of the game is to color as many cells as possible");
  moveto 395 660;
  draw_string ("Select a tile, transform it, and select a space on the board to place");

  (*QUIT Button*)
  set_color red;
  fill_rect 540 590 120 60;
  set_color black;
  moveto 590 615; draw_string ("QUIT");


  (*Board setup*)
  (* for x = 0 to 15
  do
    for y = 0 to 15 do
      let pt1 = 400 + (25 * (x)) in
      let pt2 = 575 - (25 * (y+1)) in
      draw_rect pt1 pt2 25 25;
      (* moveto pt1 pt2;
      draw_string ("("^(string_of_int x)^", "^(string_of_int y)^")"); *)
    done;
  done; *)

  (*Player Inventory Set Up*)

  (*Player 1 Inventory*)
  draw_rect 10 350 380 390;
  moveto 150 720; draw_string ("Player 1 Inventory");
  (*Player 2 Inventory*)
  draw_rect 810 350 380 390;
  moveto 950 720; draw_string ("Player 2 Inventory");

  (*Player 1 Buttons*)
  draw_gui_button game.gp1rti black;
  draw_gui_text 268 304 "Rotate Tile" black;

  draw_gui_button game.gp1fx black;
  draw_gui_text 230 238 "Flip X" black;

  draw_gui_button game.gp1fy black;
  draw_gui_text 325 238 "Flip Y" black;

  draw_gui_button game.gp1rot black;
  draw_gui_text 225 172 "Forfeit All Future Turns" black;

  (*Player 2 Buttons*)
  draw_gui_button game.gp2rti black;
  draw_gui_text 1065 304 "Rotate Tile" black;

  draw_gui_button game.gp2fx black;
  draw_gui_text 1030 238 "Flip X" black;

  draw_gui_button game.gp2fy black;
  draw_gui_text 1125 238 "Flip Y" black;

  draw_gui_button game.gp2rot black;
  draw_gui_text 1025 172 "Forfeit All Future Turns" black;

  (*Message Board*)
  draw_rect 400 10 400 ((750-400)/2 -20);
  draw_gui_text 550 140 "Game Status Board" black;
  draw_gui_text 530 110
    ("Current Player: "^game.state.curr_player.player_name) black;

  for i=0 to ((List.length game.state.players)-1) do
    let score= (List.nth game.state.players i).score in
    draw_gui_text 540 (80-(20*i))
      ("Player "^(string_of_int (i+1))^"'s Score: "^
       (string_of_int (score)))
      black;
  done;

  (*Writes WINNER when there is a winner.*)
  draw_gui_text 530 30 (game.gwinner) red;


  (*Player 1 Message Board*)
  draw_rect 10 10 380 120;
  draw_gui_text 140 100 "Player 1 Status Board" black;
  draw_gui_text 140 60 game.p1messages black;

  (*Player 2 Message Board*)
  draw_rect 810 10 380 120;
  draw_gui_text 940 100 "Player 2 Status Board" black;
  draw_gui_text 940 60 game.p2messages black;

  (*DRAWING IMPORTANT STUFF*)
  draw_onto_canvas_helper game.state.canvas1 0;
  draw_onto_canvas_helper game.state.canvas2 1;
  draw_onto_board (Array.to_list game.state.board);
  (*  let pt1= 10 + (60* (x+1)) + (800*(player_id)) in
      let pt2= 142 + ((200/3) * (y+1)) in *)
  (for x = 1 to 3 do
    for y = 1 to 3 do
      let pt1 = 10 + (((200 - 20)/3) * (x-1)) in
      let pt2 = (750- 610) + ((200/3) * (y-1)) +2 in
      draw_rect pt1 pt2 (180/3) (200/3);
    done;
  done);
  (for x = 1 to 3 do
    for y = 1 to 3 do
      let pt1 = (810) + ((60)*(x-1)) in
      let pt2 = (140) + ((200/3) * (y-1)) +2 in
      draw_rect pt1 pt2 (60) (200/3);
      moveto pt1 pt2; draw_string ("P2("^(string_of_int x)^", "^(string_of_int y)^")");
    done;
  done;);
  set_color red;
  fill_ellipse  100 242 15 15;
    fill_ellipse  900 242 15 15;

  (**** CLICKER FUNCTIONS ****)
  (*[click ()] links the click in the GUI window to the gui region the click
    was in to properly process the functions of that region.*)
  let click () =
    let clicker = Graphics.wait_next_event [Button_down] in
    (clicker.mouse_x, clicker.mouse_y) in
  let (px,py)= click () in
  let rec which_button regions=
    match regions with
    | [] -> set_color black ;
    | ((x1,x2,y1,y2))::t ->
      if ((px>=10 && px<=390) && (py>=350 && py<=740)
          && (getcurrentplayer game.state) = 0 &&
          game.state.curr_player.status != Stop
          )
      then
        begin
          (* stor.message <- "You Clicked P1 Inv Reg.!"; *)
          let lst= game.gp1inv in
          (click_inventory lst px py 0)
        end
      else if ((px>=200 && px<=390) && (py>= 142 && py<=406)
              && (getcurrentplayer game.state) = 0 &&
              game.state.curr_player.status != Stop)
      then
        begin
          (* stor.message <- "You Clicked P1 Buttons Reg.!"; *)
          (* let lst = game.gp1buttons in *)
          (click_buttons_p1 px py)
        end

      else if ((px>=810 && px<=1190) && (py>=350 && py<=740)
              && (getcurrentplayer game.state) = 1 &&
              game.state.curr_player.status != Stop)
      then
        begin
          (* stor.message <- "You Clicked P2 Inv Reg.!"; *)
          let lst= game.gp2inv in
          (click_inventory lst px py 1)
        end

      else if ((px>=1000 && px<=1190) && (py>=142 && py<=340)
              && (getcurrentplayer game.state) = 1 &&
              game.state.curr_player.status != Stop)
      then
        begin
          (* stor.message <- "You Clicked P2 Buttons Reg.!"; *)
          (* let lst = game.gp2buttons in *)
          (click_buttons_p2 px py )
        end

      else if ((px>=400 && px<=800) && (py>=175 && py<=575))
      then
        begin
        (* game.p1messages <- "" ;
        game.p2messages <- "" ; *)
        place_tile_checker game.state px py;
      end
      else if ((px>=540 && px<=660) && (py>=590 && py<=650))
      then Graphics.close_graph()
      else
        which_button t;
  in
  begin
    draw_gui_text 140 60 game.p1messages black;
    draw_gui_text 940 60 game.p2messages black;

    moveto 200 200; which_button game.gregions; clear_graph(); loop ();
  end

(* [opening ()] presents the opening window of the game application.*)
let rec opening ()=
  clear_graph ();
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  moveto (xf/2) (2*yf/3);
  set_color black;
  moveto ((xf/4)+((xf/2)/2)-20) ((yf/4)+(yf/7)/2);
  draw_string "Play Now!";
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
  set_color black;
  moveto ((xf/4)+((xf/2)/2)-20) ((yf/4)+(yf/7)/2);
  draw_string "Play Now!";

  set_color red;
  draw_rect (xf/4) ((yf/4)+(yf/7)+20) (xf/2) (yf/7);
  fill_rect (xf/4) ((yf/4)+(yf/7)+20) (xf/2) (yf/7);
  set_color black;
  moveto ((xf/4)+((xf/2)/2)-20) (((yf/4)+(yf/7)+20) + (yf/7)/2);
  draw_string "Quit Game";

  (* [starting] links the buttons in the opening screen of the game to
     their respective functions.*)
  let starting =
    wait_next_event [Button_down] in
  let xf= Graphics.size_x () in
  let yf= Graphics.size_y () in
  if (*if green*)(starting.mouse_x >= xf/4
                  && starting.mouse_x<= ((xf/4) + (xf/2)))
                  && (starting.mouse_y >= yf/4
                  && starting.mouse_y<= ((yf/4) + (yf/7)))
  then loop ()
  else if (*red*)(starting.mouse_x >= xf/4 && starting.mouse_x<= ((xf/4) + (xf/2)))
                  && (starting.mouse_y >= ((yf/4)+(yf/7)+20)
                  && starting.mouse_y<= (((yf/4)+(yf/7)+20) + (yf/7)))
  then ()
  else (*no box clicked*) opening ()
