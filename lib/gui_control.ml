(* extract the colors from a rule and return them as a string *)
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

class virtual spinner rect name ~min ~max =
  object
    inherit Gui.baseObj
    val mutable value = 100
    val mutable is_selected = false

    method render =
      let value', op = Raygui.(spinner rect name value ~min ~max is_selected) in
      value <- value';
      match op with true -> is_selected <- not is_selected | false -> ()

    method recompute_dimensions _ _ = ()
    method check_hover _state _mouse_pos = ()
    method value = value
  end

class rowSpinner rect name ~min ~max =
  object (self)
    inherit spinner rect name ~min ~max
    method tick state = { state with Gamestate.rows = self#value }
  end

class colSpinner rect name ~min ~max =
  object (self)
    inherit spinner rect name ~min ~max
    method tick state = { state with Gamestate.cols = self#value }
  end

class virtual checkbox rect name =
  object
    inherit Gui.baseObj
    val mutable is_checked = false
    val mutable changed_last_tick = false

    method render =
      let is_checked' = Raygui.(check_box rect name is_checked) in
      changed_last_tick <- is_checked <> is_checked';
      is_checked <- is_checked'

    method recompute_dimensions _ _ = ()
    method check_hover _state _mouse_pos = ()
    method is_checked = is_checked
  end

class pausedCheckbox rect name =
  object (self)
    inherit checkbox rect name

    method tick state =
      if changed_last_tick then
        { state with Gamestate.paused = self#is_checked }
      else (
        is_checked <- state.Gamestate.paused;
        state)
  end

class virtual slider rect ~min ~max ~default =
  object
    inherit Gui.baseObj
    val mutable value = default
    val mutable is_selected = false

    method render =
      let current_value =
        value *. 100. |> Float.round |> Fun.flip ( /. ) 100. |> Float.to_string
      in
      let value' =
        Raygui.(slider rect current_value (Float.to_string max) value ~min ~max)
      in
      value <- value'

    method recompute_dimensions _ _ = ()
    method check_hover _state _mouse_pos = ()
    method value = value
  end

class speedSlider rect ~min ~max ~default =
  object (self)
    inherit slider rect ~min ~max ~default
    method tick state = { state with Gamestate.step_intervall = self#value }
  end

class virtual button rect name =
  object
    inherit Gui.baseObj
    val mutable is_pressed = false

    method render =
      let is_pressed' = Raygui.(button rect name) in
      is_pressed <- is_pressed'

    method recompute_dimensions _ _ = ()
    method check_hover _state _mouse_pos = ()
    method is_pressed = is_pressed
  end

class resetButton rect name =
  object (self)
    inherit button rect name
    method tick state = { state with Gamestate.reset = self#is_pressed }
  end

class randomButton rect name =
  object (self)
    inherit button rect name
    method tick state = { state with Gamestate.random = self#is_pressed }
  end
