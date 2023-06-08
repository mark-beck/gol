let run_step (cells : Cell.t array array) =
  cells
  |> Array.mapi (fun i row ->
         row
         |> Array.mapi (fun j cell ->
                let state =
                  match Cell.state cell with
                  | 0 -> (
                      match Board.count_nbs cells i j with 3 -> 1 | _ -> 0)
                  | 1 -> (
                      match Board.count_nbs cells i j with 2 | 3 -> 1 | _ -> 0)
                  | a -> a
                in
                { cell with state }))
