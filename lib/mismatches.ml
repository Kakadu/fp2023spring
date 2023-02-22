open Printf
open Base

(* let f flg = if flg then fun x y -> x * y else fun x y -> y *)

type rectangle = { x1 : int; y1 : int; x2 : int; y2 : int }

let rectangle ~x1 ~x2 ~y1 ~y2 = { x1; x2; y1; y2 }

let rectangle_2 ~x1 ~y1 ~width ~height =
  { x1; y1; x2 = x1 + width; y2 = y1 + height }

let _ = function { x1; x2; y2; y1 } -> x1 + x2 + y1 + y2
let empty_rectangle = rectangle_2 ~width:0
let _ = rectangle ~y2:2 ~x1:2 ~y1:2 ~x2:2

let find_mismatches cmp map1 map2 =
  Sequence.filter_map (Map.to_sequence map1) ~f:(fun (key, data) ->
      match Map.find map2 key with
      | None -> None
      | Some data2 -> if cmp (data, data2) then Some key else None)

let%expect_test " " =
  let map1 =
    Map.of_alist_exn (module String) [ ("a", 1); ("b", 4777); ("c", 3) ]
  in
  let map2 =
    Map.of_alist_exn (module String) [ ("a", 1); ("b", 4); ("c", 13) ]
  in
  find_mismatches (fun (a, b) -> a <> b) map1 map2
  |> Sequence.iter ~f:(printf "%s ");
  [%expect {| b c |}]
