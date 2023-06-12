open Gol
open Gamestate

let width = 680
let height = 1000

let init_game_objects () =
  let open Raylib in
  let to_baseObj ob = (ob :> Gui.baseObj) in
  let rule = Rule.bb in
  let board_obj =
    to_baseObj
    @@ new Gui.boardObj 0 150 400 400 (Board.create_clear 100 100) rule
  in

  (* set up control elements *)
  let paused_checkbox =
    to_baseObj
    @@ new Gui_control.pausedCheckbox
         (Rectangle.create 50.0 25.0 15.0 15.0)
         "paused"
  in
  let speed_slider =
    to_baseObj
    @@ new Gui_control.speedSlider
         (Rectangle.create 50.0 50. 200.0 20.0)
         ~min:0. ~max:1. ~default:0.2
  in
  let reset_button =
    to_baseObj
    @@ new Gui_control.resetButton (Rectangle.create 50.0 75. 50.0 20.0) "reset"
  in
  let random_button =
    to_baseObj
    @@ new Gui_control.randomButton
         (Rectangle.create 125.0 75. 50.0 20.0)
         "random"
  in
  let row_spinner =
    to_baseObj
    @@ new Gui_control.rowSpinner
         (Rectangle.create 50.0 100. 100.0 20.0)
         "rows" ~min:0 ~max:300
  in
  let col_spinner =
    to_baseObj
    @@ new Gui_control.colSpinner
         (Rectangle.create 225.0 100. 100.0 20.0)
         "cols" ~min:0 ~max:300
  in

  let selector =
    to_baseObj
    @@ new Gui_control.colorSelector
         (Raylib.Rectangle.create 250. 25. 100. 20.)
         rule
  in
  [
    board_obj;
    row_spinner;
    col_spinner;
    paused_checkbox;
    speed_slider;
    reset_button;
    random_button;
    selector;
  ]

let setup () =
  Raylib.init_window width height "gol";
  Raylib.set_target_fps 60

let gamestate = Gamestate.default
let gameObjs = init_game_objects ()

let rec loop gamestate =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      let gamestate = { gamestate with tick_count = Raylib.get_time () } in

      let screen_height = get_screen_height () in
      let screen_width = get_screen_width () in

      (* resize game objects *)
      gameObjs
      |> List.iter (fun obj ->
             obj#recompute_dimensions screen_width screen_height);

      (* check if mouse is over control element *)
      let mouse_pos = get_mouse_position () in

      gameObjs |> List.iter (fun obj -> obj#check_hover gamestate mouse_pos);

      (* check if pause state should be changed *)
      let gamestate =
        if is_key_pressed Key.Space then (
          print_endline "game paused/unpaused";
          { gamestate with paused = not gamestate.paused })
        else gamestate
      in

      (* update gamestate *)
      let gamestate =
        List.fold_left (fun gs obj -> obj#tick gs) gamestate gameObjs
      in

      (* print info every second *)
      let current_time = get_time () in
      let gamestate =
        if gamestate.last_info +. 10. < current_time then (
          print_string "fps: ";
          print_endline @@ string_of_int @@ get_fps ();
          print_string "frametime: ";
          print_endline @@ string_of_float @@ get_frame_time ();
          print_string "width: ";
          print_endline @@ string_of_int @@ get_screen_width ();
          print_string "height: ";
          print_endline @@ string_of_int @@ get_screen_height ();
          print_string "selected color: ";
          print_endline @@ string_of_int @@ gamestate.selected_color;
          { gamestate with last_info = current_time })
        else gamestate
      in

      begin_drawing ();

      draw_fps (screen_width - 75) 0;

      clear_background Color.raywhite;
      gameObjs |> List.iter (fun obj -> obj#render);
      end_drawing ();
      loop gamestate

let () =
  setup ();
  loop gamestate
