class virtual gameObj :
  object
    method virtual check_hover : Raylib.Vector2.t -> unit
    method virtual recompute_dimensions : int -> int -> unit
    method virtual render : unit
    method virtual tick : unit
  end

class boardObj :
  int
  -> int
  -> int
  -> int
  -> Board.t
  -> Rule.t
  -> object
       method check_hover : Raylib.Vector2.t -> unit
       method clear_board : int -> int -> unit
       method random_board : int -> int -> unit
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method tick : unit
     end
