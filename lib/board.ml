type t = Cell.t array array

let rows board = Array.length board
let cols board = Array.get board 0 |> Array.length
let create_clear x y = Array.make x @@ Array.make y @@ Cell.empty ()

let map2d f arrays =
  arrays |> Array.map @@ fun row -> row |> Array.map @@ fun e -> f e

let create_random ?(prob = 20) x y =
  create_clear x y
  |> map2d @@ fun cell ->
     if Random.int 100 < prob then { cell with Cell.state = 1 } else cell

let get_2d xy arrays =
  let x, y = xy in
  try
    let row = Array.get arrays x in
    Some (Array.get row y)
  with Invalid_argument _ -> None

let count_nbs cells x y =
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

let run_step cells =
  cells
  |> Array.mapi (fun i row ->
         row
         |> Array.mapi (fun j cell ->
                let state =
                  match Cell.state cell with
                  | 0 -> ( match count_nbs cells i j with 3 -> 1 | _ -> 0)
                  | 1 -> ( match count_nbs cells i j with 2 | 3 -> 1 | _ -> 0)
                  | a -> a
                in
                { cell with state }))
