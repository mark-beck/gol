open Helpers
module Smap = Map.Make (String)

type t = {
  rule_map : int Smap.t array;
  default : int array;
  colors : string array;
}

let compare_with_stars s1 s2 =
  let s1 = String.to_seq s1 in
  let s2 = String.to_seq s2 in
  Seq.zip s1 s2 |> Seq.for_all (fun (c1, c2) -> c1 = c2 || c1 = '*' || c2 = '*')

let find_most_fitting rule nbs =
  rule
  |> Smap.find_first_opt (fun key -> compare_with_stars key nbs)
  |> Option.map snd

let apply_rule (rule : t) (nbs : int list) (state : int) =
  let nbs_str = nbs |> List.map Int.to_string |> List.fold_left String.cat "" in
  let rule_map = Array.get rule.rule_map state in
  rule_map |> Smap.find_opt nbs_str
  |> oor (find_most_fitting rule_map nbs_str)
  |> Option.value ~default:(Array.get rule.default state)

let run_step (rule : t) (cells : Board.t) =
  cells
  |> Array.mapi (fun i row ->
         row
         |> Array.mapi (fun j cell ->
                let states_number = Array.length rule.default in
                let nbs =
                  List.init states_number id
                  |> List.map (fun state -> Board.count_nbs cells state i j)
                in
                let state = apply_rule rule nbs (Cell.state cell) in
                Cell.make state))

let color rule state = Array.get rule.colors state

let gol =
  let dead = [ ("53", 1) ] |> List.to_seq |> Smap.of_seq in
  let alive = [ ("62", 1); ("53", 1) ] |> List.to_seq |> Smap.of_seq in
  let rule_map = [ dead; alive ] |> Array.of_list in
  let default = [ 0; 0 ] |> Array.of_list in
  let colors = [ "gray"; "black" ] |> Array.of_list in
  { rule_map; default; colors }

let seeds =
  let dead = [ ("62", 1) ] |> List.to_seq |> Smap.of_seq in
  let alive = [] |> List.to_seq |> Smap.of_seq in
  let rule_map = [ dead; alive ] |> Array.of_list in
  let default = [ 0; 0 ] |> Array.of_list in
  let colors = [ "gray"; "black" ] |> Array.of_list in
  { rule_map; default; colors }

let bb =
  let dead = [ ("*2*", 1) ] |> List.to_seq |> Smap.of_seq in
  let dying = [] |> List.to_seq |> Smap.of_seq in
  let alive = [] |> List.to_seq |> Smap.of_seq in
  let rule_map = [ dead; alive; dying ] |> Array.of_list in
  let default = [ 0; 2; 0 ] |> Array.of_list in
  let colors = [ "gray"; "black"; "blue" ] |> Array.of_list in
  { rule_map; default; colors }
