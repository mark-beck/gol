module Smap = Map.Make (String)

let extract_colors rule =
  let res =
    List.init (Rule.states rule) (fun i -> Rule.color rule i)
    |> List.fold_left (fun acc e -> acc ^ "\n" ^ e) ""
    |> String.trim
  in
  Printf.printf "colors: %s\n" res;
  res

class colorSelector rect rule =
  object
    inherit Gui.baseObj
    val mutable is_open = false
    val mutable selected_state = 0
    val colors = extract_colors rule

    method render =
      let selected, op =
        Raygui.(dropdown_box rect colors selected_state is_open)
      in
      selected_state <- selected;
      match op with true -> is_open <- not is_open | false -> ()

    method recompute_dimensions _ _ = ()
    method check_hover _state _mouse_pos = ()
    method tick state = { state with Gamestate.selected_color = selected_state }
  end

class virtual spinner rect name =
  object
    inherit Gui.baseObj
    val mutable value = 100
    val mutable is_selected = false

    method render =
      let value', op =
        Raygui.(spinner rect name value ~min:0 ~max:200 is_selected)
      in
      value <- value';
      match op with true -> is_selected <- not is_selected | false -> ()

    method recompute_dimensions _ _ = ()
    method check_hover _state _mouse_pos = ()
    method value = value
  end

class rowSpinner rect name =
  object (self)
    inherit spinner rect name
    method tick state = { state with Gamestate.rows = self#value }
  end

class colSpinner rect name =
  object (self)
    inherit spinner rect name
    method tick state = { state with Gamestate.cols = self#value }
  end
