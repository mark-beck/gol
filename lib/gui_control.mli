(** This module contains the definitions of all the control UI elements. *)

(** A color selector is a UI element that allows the user to select a color
    to draw *)
class colorSelector :
  Raylib.Rectangle.t
  -> Rule.t
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
     end

(** A row spinner is a UI element that allows the user to set the number of
    rows in the grid *)
class rowSpinner :
  Raylib.Rectangle.t
  -> string
  -> min:int
  -> max:int
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
       method value : int
     end

(** A col spinner is a UI element that allows the user to set the number of
    columns in the grid *)
class colSpinner :
  Raylib.Rectangle.t
  -> string
  -> min:int
  -> max:int
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
       method value : int
     end

(** A paused checkbox is a UI element that allows the user to pause the
    simulation *)
class pausedCheckbox :
  Raylib.Rectangle.t
  -> string
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method is_checked : bool
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
     end

(** A speed slider is a UI element that allows the user to set the speed of
    the simulation *)
class speedSlider :
  Raylib.Rectangle.t
  -> min:float
  -> max:float
  -> default:float
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
       method value : float
     end

(** A reset button is a UI element that allows the user to reset the
    simulation *)
class resetButton :
  Raylib.Rectangle.t
  -> string
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method is_pressed : bool
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
     end

(** A random button is a UI element that allows the user to generate a random
    grid *)
class randomButton :
  Raylib.Rectangle.t
  -> string
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method is_pressed : bool
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : Gamestate.t -> Gamestate.t
     end
