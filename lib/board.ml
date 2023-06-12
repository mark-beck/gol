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
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  |> List.to_seq
  |> Seq.map (fun (i, j) -> (x + i, y + j))
  |> Seq.map (fun xy -> Helpers.get_2d xy cells)
  |> Seq.map (function Some v when v = state -> 1 | _ -> 0)
  |> Seq.fold_left ( + ) 0

let count_nbs_2 cells n x y =
  let nb_array = Array.make n 0 in
  let neighbors =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  List.iter
    (fun (i, j) ->
      let nx = x + i in
      let ny = y + j in
      match Helpers.get_2d (nx, ny) cells with
      | Some v -> Array.unsafe_set nb_array v ((Array.unsafe_get nb_array v) + 1)  (* this is safe as long as the ruleset for the board never changes *)
      | _ -> ())
    neighbors;
  nb_array

let get = get_2d
let map = map2d

let mapi f board =
  board
  |> Array.mapi @@ fun i row -> row |> Array.mapi @@ fun j cell -> f (i, j) cell
