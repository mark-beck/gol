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

let map2d f arrays =
  arrays |> Array.map @@ fun row -> row |> Array.map @@ fun e -> f e

let parse_color = function
  | "white" -> Raylib.Color.white
  | "black" -> Raylib.Color.black
  | "gray" -> Raylib.Color.gray
  | "red" -> Raylib.Color.red
  | "green" -> Raylib.Color.green
  | "blue" -> Raylib.Color.blue
  | "yellow" -> Raylib.Color.yellow
  | "orange" -> Raylib.Color.orange
  | "pink" -> Raylib.Color.pink
  | _ -> Raylib.Color.white

let id x = x
let oor v opt = match opt with None -> v | r -> r
