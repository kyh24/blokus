open Gui
open Graphics

let rec main () =
    open_graph " 1200x750";
    Graphics.set_window_title "bLoKaML by Srishti B., Kati H., Sahithi K., & Devki T.";
    Gui.main ();
    close_graph ()


let () = main ()
