type t = { state : int; x : int; y : int; width : int }

let state cell = cell.state
let empty () = { state = 0; x = 0; y = 0; width = 0 }

let is_overlapping cell vec =
  let point_x = Raylib.Vector2.x vec |> int_of_float in
  let point_y = Raylib.Vector2.y vec |> int_of_float in
  point_x >= cell.x
  && point_x < cell.x + cell.width
  && point_y >= cell.y
  && point_y < cell.y + cell.width
