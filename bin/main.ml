module Cell = struct
  type t = {
    state: int;
    x: int;
    y: int;
    width: int;
  }

  let state cell = cell.state
  
  let empty () = { state = 0; x = 0; y = 0; width = 0; }

  let is_overlapping cell vec =
    let point_x = Raylib.Vector2.x vec |> int_of_float in
    let point_y = Raylib.Vector2.y vec |> int_of_float in
    point_x >= cell.x && point_x < (cell.x + cell.width) && point_y >= cell.y && point_y < (cell.y + cell.width)

  let draw cell =
    let open Raylib in
    let color = match cell.state with
    | 0 -> Color.gray
    | 1 -> Color.black
    | _ -> Color.red
    in
    draw_rectangle cell.x cell.y cell.width cell.width color


end


let get_2d xy arrays =
  let (x, y) = xy in
  try let row = Array.get arrays x in
  Some ( Array.get row y)
with Invalid_argument _ -> None 

let nb_count cells x y =
  (*List of positions around*)
  let poses = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1);] in
  poses 
  |> List.map (fun (i, j) -> (x + i, y + j) ) 
  |> List.map (fun xy -> get_2d xy cells) 
  |> List.map (fun o -> o |> Option.map (fun e -> Cell.state e ))
  |> List.map (function | Some 1 -> 1 | _ -> 0)
  |> List.fold_left (+) 0
  



let run_step (cells : Cell.t array array)  =
  cells |> Array.mapi (fun i row -> row |> Array.mapi (fun j cell -> 
    { cell with Cell.state = 
      match Cell.state cell with
      | 0 -> begin match nb_count cells i j with
        | 3 -> 1
        | _ -> 0
        end
      | 1 -> begin match nb_count cells i j with
        | 2 | 3 -> 1
        | _ -> 0
      end
      | a -> a
    }))
      

let width = 800
let height = 450

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
}

let gamestate = {
  cells = Array.make 100 (Array.make 100 (Cell.empty ()));
  last_gamestep = 0.0;
  paused = true;
  step_intervall = 0.2;
  reset = false;
  random = false;
}

let init_random () =
  Array.make 100 (Array.make 100 (Cell.empty ())) |> Array.map (fun row -> row |> Array.map (fun cell ->
    if Random.int 100 < 20
      then { cell with Cell.state = 1} else cell))


let rec loop gamestate =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in

      (* check reset *)
      let gamestate = if gamestate.reset
        then { gamestate with cells = Array.make 100 (Array.make 100 (Cell.empty ())); reset = false; }
        else gamestate in

      let gamestate = if gamestate.random
        then { gamestate with cells = init_random (); random = false; }
        else gamestate in

      (* compute cell positions *)
      let screen_height = get_screen_height () in
      let screen_width = get_screen_width () in
      (* keep 150 px reserved for controls *)
      let grid_width = Int.min (screen_height - 150) screen_width in
      let cells_count = Array.length gamestate.cells in
      let single_cell_len = grid_width / cells_count in
      let gamestate = {gamestate with cells = gamestate.cells |> Array.mapi (fun i row -> row |> Array.mapi (fun j cell -> 
        {
          cell with 
          Cell.x = i * single_cell_len; 
          Cell.y = j * single_cell_len;
          Cell.width = single_cell_len; 
        })) }
      in

      (* compute controls positions *)
      let paused_checkbox_pos = Rectangle.create 25.0 (float_of_int grid_width) 15.0 15.0 in
      let step_spinner_pos = Rectangle.create 25.0 ((float_of_int grid_width) +. 25.0) 200.0 20.0 in
      let reset_button_pos = Rectangle.create 25.0 ((float_of_int grid_width) +. 50.0) 50.0 20.0 in
      let random_button_pos = Rectangle.create 100.0 ((float_of_int grid_width) +. 50.0) 50.0 20.0 in

      (* check if mouse is over cell and update the cell*)
      let mouse_pos = get_mouse_position () in
      let gamestate = { gamestate with cells = gamestate.cells |> Array.map (fun row -> row |> Array.map (fun cell -> 
        if Cell.is_overlapping cell mouse_pos 
        then begin 
          if is_mouse_button_down MouseButton.Left then { cell with Cell.state = 1 }
          else if is_mouse_button_down MouseButton.Right then { cell with Cell.state = 0 }
          else cell
        end
        else cell ))} in

      (* check if pause state should be changed *)
      let current_time = get_time () in
      let gamestate = if is_key_pressed Key.Space
        then (
          print_endline "game paused/unpaused";
          { gamestate with paused = not gamestate.paused }
        )
        else gamestate in

      (* check if game should be updated. *)
      let gamestate =  if not gamestate.paused && gamestate.last_gamestep +. gamestate.step_intervall < current_time 
        then { gamestate with cells = run_step gamestate.cells; last_gamestep = current_time }
        else gamestate in
      
      

      begin_drawing ();
      clear_background Color.raywhite;
      gamestate.cells |> Array.iter
        (fun row ->
          row |> Array.iter
            (fun cell ->
              Cell.draw cell));
      
      (* render controls *)
      Raygui.enable ();
      let paused = Raygui.(check_box paused_checkbox_pos "PAUSED" gamestate.paused) in
      let step_intervall = Raygui.(slider step_spinner_pos "0" "1" gamestate.step_intervall ~min:0. ~max:1.) in
      let reset = Raygui.(button reset_button_pos "Reset") in
      let random = Raygui.(button random_button_pos "Random") in
      let gamestate = { gamestate with paused; step_intervall; reset; random; }
      in

      end_drawing ();
      loop gamestate

let () =
  setup ();
  loop gamestate