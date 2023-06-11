open Helpers

type t = int array array

let rows board = Array.length board
let cols board = Array.get board 0 |> Array.length
let create_clear x y = Array.make x @@ Array.make y 0

let create_random ?(prob = 20) x y =
  create_clear x y
  |> map2d @@ fun cell -> if Random.int 100 < prob then 1 else cell

let count_nbs cells state x y =
  (*List of positions around*)
  let poses =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  poses
  |> List.map (fun (i, j) -> (x + i, y + j))
  |> List.map (fun xy -> Helpers.get_2d xy cells)
  |> List.map (function Some v when v = state -> 1 | _ -> 0)
  |> List.fold_left ( + ) 0

let get = get_2d
let map = map2d

let mapi f board =
  board
  |> Array.mapi @@ fun i row -> row |> Array.mapi @@ fun j cell -> f (i, j) cell
