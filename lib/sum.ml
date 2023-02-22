let rec sum list =
  match list with
  | [] -> 0
  | h :: tl -> h + sum tl
;;

open Printf

let%expect_test " " =
  printf "%d\n" (sum [ 1; 2; 3; 4 ]);
  printf "Success";
  [%expect {|
    10
    Success |}]
;;
