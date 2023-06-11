type t = {
  tick_count : float;
  last_gamestep : float;
  paused : bool;
  step_intervall : float;
  reset : bool;
  random : bool;
  last_info : float;
  rows : int;
  cols : int;
  selected_color : int;
}

let default =
  {
    tick_count = 0.0;
    last_gamestep = 0.0;
    paused = true;
    step_intervall = 0.2;
    reset = false;
    random = false;
    last_info = 0.;
    rows = 100;
    cols = 100;
    selected_color = 1;
  }
