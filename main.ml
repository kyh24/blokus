open Graphics
open Printf


let rec loop () =
  let e = wait_next_event [Mouse_motion; Key_pressed] in

  let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in


  clear_graph ();
  moveto 0 0; draw_string mouse_description;

  if e.key <> 'q' then loop () else ()

let () =
  open_graph " 700x700";
  Graphics.set_window_title "bLoKaML by Srishti B., Kati H., Sahithi K., & Devki T.";
  loop ();
  close_graph ();
