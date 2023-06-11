open Gol
open Gol.Helpers

let width = 680
let height = 1000

let setup () =
  Raylib.init_window width height "raylib [core] example - mouse input";
  Raylib.set_target_fps 60

type gamestate = {
  last_gamestep : float;
  paused : bool;
  step_intervall : float;
  reset : bool;
  random : bool;
  last_info : float;
  rows : int;
  cols : int;
}

let gamestate =
  {
    last_gamestep = 0.0;
    paused = true;
    step_intervall = 0.2;
    reset = false;
    random = false;
    last_info = 0.;
    rows = 100;
    cols = 100;
  }

let boardObj =
  new Renderable.boardObj 0 150 400 400 (Board.create_clear 100 100) Rule.bb

let clear_board_ref rows cols = boardObj#clear_board rows cols
let random_board_ref rows cols = boardObj#random_board rows cols
let gameObjs = [ boardObj ]

let rec loop gamestate =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      (* check reset *)
      let gamestate =
        if gamestate.reset then (
          clear_board_ref gamestate.rows gamestate.cols;
          { gamestate with reset = false })
        else gamestate
      in

      let gamestate =
        if gamestate.random then (
          random_board_ref gamestate.rows gamestate.cols;
          { gamestate with random = false })
        else gamestate
      in

      (* compute cell positions *)
      let screen_height = get_screen_height () in
      let screen_width = get_screen_width () in

      gameObjs
      |> List.iter @@ fun obj ->
         obj#recompute_dimensions screen_width screen_height;

         (* compute controls positions *)
         let paused_checkbox_pos = Rectangle.create 25.0 25.0 15.0 15.0 in
         let step_spinner_pos = Rectangle.create 25.0 50. 200.0 20.0 in
         let reset_button_pos = Rectangle.create 25.0 75. 50.0 20.0 in
         let random_button_pos = Rectangle.create 100.0 75. 50.0 20.0 in
         let rows_spinner_pos = Rectangle.create 25.0 100. 100.0 20.0 in
         let cols_spinner_pos = Rectangle.create 200.0 100. 100.0 20.0 in

         (* check if mouse is over control element *)
         let mouse_pos = get_mouse_position () in
         let rows_spinner_selected = vec2d_in_rect mouse_pos rows_spinner_pos in
         let cols_spinner_selected = vec2d_in_rect mouse_pos cols_spinner_pos in

         gameObjs
         |> List.iter @@ fun obj ->
            obj#check_hover mouse_pos;

            (* check if pause state should be changed *)
            let current_time = get_time () in
            let gamestate =
              if is_key_pressed Key.Space then (
                print_endline "game paused/unpaused";
                { gamestate with paused = not gamestate.paused })
              else gamestate
            in

            (* check if game should be updated. *)
            let gamestate =
              if
                (not gamestate.paused)
                && gamestate.last_gamestep +. gamestate.step_intervall
                   < current_time
              then (
                gameObjs |> List.iter (fun obj -> obj#tick);
                { gamestate with last_gamestep = current_time })
              else gamestate
            in

            (* print info every second *)
            let gamestate =
              if gamestate.last_info +. 1. < current_time then (
                print_string "fps: ";
                print_endline @@ string_of_int @@ get_fps ();
                print_string "frametime: ";
                print_endline @@ string_of_float @@ get_frame_time ();
                print_string "width: ";
                print_endline @@ string_of_int @@ get_screen_width ();
                print_string "height: ";
                print_endline @@ string_of_int @@ get_screen_height ();
                { gamestate with last_info = current_time })
              else gamestate
            in

            begin_drawing ();

            draw_fps (screen_width - 75) 0;

            clear_background Color.raywhite;
            gameObjs
            |> List.iter @@ fun obj ->
               obj#render;

               (* render controls *)
               Raygui.enable ();
               let paused =
                 Raygui.(
                   check_box paused_checkbox_pos "PAUSED" gamestate.paused)
               in
               let step_intervall =
                 Raygui.(
                   slider step_spinner_pos "0" "1" gamestate.step_intervall
                     ~min:0. ~max:1.)
               in
               let reset = Raygui.(button reset_button_pos "Reset") in
               let random = Raygui.(button random_button_pos "Random") in
               let rows, _ =
                 Raygui.(
                   spinner rows_spinner_pos "rows" gamestate.rows ~min:0
                     ~max:200 rows_spinner_selected)
               in
               let cols, _ =
                 Raygui.(
                   spinner cols_spinner_pos "cols" gamestate.cols ~min:0
                     ~max:200 cols_spinner_selected)
               in
               let gamestate =
                 {
                   gamestate with
                   paused;
                   step_intervall;
                   reset;
                   random;
                   rows;
                   cols;
                 }
               in

               end_drawing ();
               loop gamestate

let () =
  setup ();
  loop gamestate
