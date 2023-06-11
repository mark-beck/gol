class virtual baseObj =
  object
    method virtual render : unit
    method virtual recompute_dimensions : int -> int -> unit
    method virtual check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
    method virtual tick : Gamestate.t -> Gamestate.t
  end

class squareObj x_pos y_pos width =
  object
    method render color =
      let open Raylib in
      draw_rectangle x_pos y_pos width width color

    method is_overlapping vec =
      let point_x = Raylib.Vector2.x vec |> int_of_float in
      let point_y = Raylib.Vector2.y vec |> int_of_float in
      point_x >= x_pos
      && point_x < x_pos + width
      && point_y >= y_pos
      && point_y < y_pos + width
  end

let init_cell_pos x y w h x_amount y_amount =
  let cell_len_with_width = w / x_amount in
  let cell_len_with_height = h / y_amount in
  let single_cell_len = Int.min cell_len_with_width cell_len_with_height in
  Array.make x_amount @@ Array.make y_amount ()
  |> Array.mapi @@ fun i row ->
     row
     |> Array.mapi @@ fun j _ ->
        let x = x + (i * single_cell_len) in
        let y = y + (j * single_cell_len) in
        new squareObj x y single_cell_len

class boardObj x_pos y_pos w h board' rule =
  object
    inherit baseObj
    val mutable board = board'

    val mutable cells_pos =
      init_cell_pos x_pos y_pos w h (Board.rows board') (Board.cols board')

    method render =
      cells_pos
      |> Array.iteri @@ fun i row ->
         row
         |> Array.iteri @@ fun j cell_obj ->
            try
              let cell = Option.get @@ Board.get (i, j) board in
              let color = Rule.color rule cell |> Helpers.parse_color in
              cell_obj#render color
            with e ->
              print_string "Error rendering cell: ";
              print_endline @@ Printexc.to_string e;
              raise e

    method recompute_dimensions w h =
      cells_pos <-
        init_cell_pos x_pos y_pos w h (Board.rows board) (Board.cols board)

    method check_hover state mouse_pos =
      let open Raylib in
      (* check if mouse is over cell and update the cell*)
      board <-
        (board
        |> Board.mapi @@ fun pos cell ->
           try
             let cellObj = Option.get @@ Helpers.get_2d pos cells_pos in
             if cellObj#is_overlapping mouse_pos then
               if is_mouse_button_down MouseButton.Left then
                 state.Gamestate.selected_color
               else if is_mouse_button_down MouseButton.Right then 0
               else cell
             else cell
           with e ->
             print_string "Error checking hover: ";
             print_endline @@ Printexc.to_string e;
             raise e)

    method tick state =
      let open Gamestate in
      (* check if board should be reset *)
      let state =
        if state.reset then (
          board <- Board.create_clear state.rows state.cols;
          cells_pos <-
            init_cell_pos x_pos y_pos w h (Board.rows board) (Board.cols board);
          { state with reset = false })
        else state
      in

      (* check if its time for a gamestep *)
      if
        (not state.Gamestate.paused)
        && Gamestate.(
             state.tick_count > state.last_gamestep +. state.step_intervall)
      then (
        board <- Rule.run_step rule board;
        { state with last_gamestep = state.tick_count })
      else state

    method clear_board rows cols = board <- Board.create_clear rows cols
    method random_board rows cols = board <- Board.create_random rows cols
    method rule = rule
  end
