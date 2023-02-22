let x = 5

module M = struct
  type cmp = LT | EQ | GT
end

let int_of_cmp arg = match arg with M.LT -> -1 | EQ -> 0 | _ -> 1

let compare_cmp a b =
  match (a, b) with
  | M.LT, M.LT | EQ, EQ | GT, GT -> M.EQ
  | LT, _ | _, GT -> LT
  | _, LT | GT, _ -> GT
(* | EQ, LT -> GT *)

(* | _ -> assert false *)

(* type 'a option = None | Some of 'a *)

let%test _ =
  match List.assoc_opt 1 [ (1, "asdf") ] with
  | None -> false
  | Some v -> v = "asdf"

let%test _ = true

(* Carrier :
    construct::::   And : phormula * phormula -> phormula
    extract_name ::::: (function Var {name} -> name | _ -> assert false )
    if (x: phormula) exist C in { True, False, ..., Impl }: x === C (..., ...)
    if x==y then
       exists C, exists n:  x=== C (a_1, ..., a_n) and y === C (b1, ..., b_n), and a_i===b_i for i=1..n
*)

type phormula =
  | True
  | False
  | Var of { name : string }
  | Not of phormula
  | Or of phormula * phormula
  | And of phormula * phormula
  | Impl of phormula * phormula

let rec eval lookup f =
  match f with
  | True -> true
  | False -> false
  | Var { name = s } -> lookup s
  | Not f -> not (eval lookup f)
  | Or (l, r) -> eval lookup l || eval lookup r
  | And (l, r) -> eval lookup l && eval lookup r
  | Impl (l, r) -> if eval lookup l then eval lookup r else true

let%test _ =
  eval
    (function "a" -> false | _ -> assert false)
    (Impl (Var { name = "a" }, Var { name = "b" }))
