open Gol

let vec2d_in_rect vec rect =
  let open Raylib in
  let vecx = Vector2.x vec in
  let vecy = Vector2.y vec in
  let rectx = Rectangle.x rect in
  let recty = Rectangle.y rect in
  let rectw = Rectangle.width rect in
  let recth = Rectangle.height rect in
  vecx >= rectx
  && vecx <= rectx +. rectw
  && vecy >= recty
  && vecy <= recty +. recth

let get_2d xy arrays =
  let x, y = xy in
  try
    let row = Array.get arrays x in
    Some (Array.get row y)
  with Invalid_argument _ -> None

let nb_count cells x y =
  (*List of positions around*)
  let poses =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  poses
  |> List.map (fun (i, j) -> (x + i, y + j))
  |> List.map (fun xy -> get_2d xy cells)
  |> List.map (fun o -> o |> Option.map (fun e -> Cell.state e))
  |> List.map (function Some 1 -> 1 | _ -> 0)
  |> List.fold_left ( + ) 0

let run_step (cells : Cell.t array array) =
  cells
  |> Array.mapi (fun i row ->
         row
         |> Array.mapi (fun j cell ->
                {
                  cell with
                  Cell.state =
                    (match Cell.state cell with
                    | 0 -> ( match nb_count cells i j with 3 -> 1 | _ -> 0)
                    | 1 -> (
                        match nb_count cells i j with 2 | 3 -> 1 | _ -> 0)
                    | a -> a);
                }))

let width = 100
let height = 100

let setup () =
  Raylib.init_window width height "raylib [core] example - mouse input";
  Raylib.set_target_fps 60

type gamestate = {
  cells : Cell.t array array;
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
    cells = Array.make 100 (Array.make 100 (Cell.empty ()));
    last_gamestep = 0.0;
    paused = true;
    step_intervall = 0.2;
    reset = false;
    random = false;
    last_info = 0.;
    rows = 100;
    cols = 100;
  }

let init_random xs ys =
  Array.make xs (Array.make ys (Cell.empty ()))
  |> Array.map (fun row ->
         row
         |> Array.map (fun cell ->
                if Random.int 100 < 20 then { cell with Cell.state = 1 }
                else cell))

let rec loop gamestate =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      (* check reset *)
      let gamestate =
        if gamestate.reset then
          {
            gamestate with
            cells =
              Array.make gamestate.cols
                (Array.make gamestate.rows (Cell.empty ()));
            reset = false;
          }
        else gamestate
      in

      let gamestate =
        if gamestate.random then
          {
            gamestate with
            cells = init_random gamestate.cols gamestate.rows;
            random = false;
          }
        else gamestate
      in

      (* compute cell positions *)
      let screen_height = get_screen_height () in
      let screen_width = get_screen_width () in

      (* keep 150 px reserved for controls *)
      let cells_count_x = Array.length gamestate.cells in
      let cells_count_y = Array.get gamestate.cells 0 |> Array.length in
      let cell_len_with_width = screen_width / cells_count_x in
      let cell_len_with_height = (screen_height - 150) / cells_count_y in
      let single_cell_len = Int.min cell_len_with_width cell_len_with_height in

      let gamestate =
        {
          gamestate with
          cells =
            gamestate.cells
            |> Array.mapi (fun i row ->
                   row
                   |> Array.mapi (fun j cell ->
                          {
                            cell with
                            Cell.x = i * single_cell_len;
                            Cell.y = (j * single_cell_len) + 150;
                            Cell.width = single_cell_len;
                          }));
        }
      in

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

      (* check if mouse is over cell and update the cell*)
      let gamestate =
        {
          gamestate with
          cells =
            gamestate.cells
            |> Array.map (fun row ->
                   row
                   |> Array.map (fun cell ->
                          if Cell.is_overlapping cell mouse_pos then
                            if is_mouse_button_down MouseButton.Left then
                              { cell with Cell.state = 1 }
                            else if is_mouse_button_down MouseButton.Right then
                              { cell with Cell.state = 0 }
                            else cell
                          else cell));
        }
      in

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
          && gamestate.last_gamestep +. gamestate.step_intervall < current_time
        then
          {
            gamestate with
            cells = run_step gamestate.cells;
            last_gamestep = current_time;
          }
        else gamestate
      in

      (* print fps every second *)
      let gamestate =
        if gamestate.last_info +. 2. < current_time then (
          print_string "current fps: ";
          print_endline @@ string_of_int @@ get_fps ();
          print_string "current frametime: ";
          print_endline @@ string_of_float @@ get_frame_time ();
          { gamestate with last_info = current_time })
        else gamestate
      in

      begin_drawing ();
      clear_background Color.raywhite;
      gamestate.cells
      |> Array.iter (fun row -> row |> Array.iter (fun cell -> Cell.draw cell));

      (* render controls *)
      Raygui.enable ();
      let paused =
        Raygui.(check_box paused_checkbox_pos "PAUSED" gamestate.paused)
      in
      let step_intervall =
        Raygui.(
          slider step_spinner_pos "0" "1" gamestate.step_intervall ~min:0.
            ~max:1.)
      in
      let reset = Raygui.(button reset_button_pos "Reset") in
      let random = Raygui.(button random_button_pos "Random") in
      let rows, _ =
        Raygui.(
          spinner rows_spinner_pos "rows" gamestate.rows ~min:0 ~max:200
            rows_spinner_selected)
      in
      let cols, _ =
        Raygui.(
          spinner cols_spinner_pos "cols" gamestate.cols ~min:0 ~max:200
            cols_spinner_selected)
      in
      let gamestate =
        { gamestate with paused; step_intervall; reset; random; rows; cols }
      in

      end_drawing ();
      loop gamestate

let () =
  setup ();
  loop gamestate
