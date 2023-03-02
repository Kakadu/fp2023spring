(* [@@@ocamlwarnerror "-32"] *)
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

type op = Or | And | Impl [@@deriving show]

type phormula =
  | True
  | False
  | Var of { name : string }
  | Not of phormula
  | Binop of op * phormula * phormula
[@@deriving show]

let rec eval lookup f =
  match f with
  | True -> true
  | False -> false
  | Var { name = s } -> lookup s
  | Not f -> not (eval lookup f)
  | Binop (Or, l, r) -> eval lookup l || eval lookup r
  | Binop (And, l, r) -> eval lookup l && eval lookup r
  | Binop (Impl, l, r) -> if eval lookup l then eval lookup r else true

(* let show_op = function Or -> "∨" | And -> "∧" | Impl -> "⇒" *)
(* let _ = ("asdf" ^ "qwe") ^ "zxcv" *)
(*
let rec show = function
  | True -> "true"
  | False -> "false"
  | Var { name } -> name
  | Not f -> "(¬ " ^ show f ^ ")"
  | Binop (op, l, r) -> "(" ^ show l ^ " " ^ show_op op ^ " " ^ show r ^ ")" *)

open Format

(* let pp_op ppf = function
     | Or -> fprintf ppf "∨"
     | And -> fprintf ppf "∧"
     | Impl -> fprintf ppf "⇒"

   let rec pp ppf = function
     | True -> fprintf ppf "true"
     | False -> fprintf ppf "false"
     | Var { name } -> fprintf ppf "%s" name
     | Not f ->
         (* fprintf ppf "(¬ %a)" pp f *)
         fprintf ppf "(¬ ";
         pp ppf f;
         fprintf ppf ")"
     | Binop (op, l, r) -> fprintf ppf "(%a %a %a)" pp l pp_op op pp r
*)
let () =
  (* print_endline (show (Binop (Impl, Var { name = "x" }, Var { name = "x" }))) *)
  Format.printf "%s %d: %a\n%!" __FILE__ __LINE__ pp_phormula
    (Binop (Impl, Var { name = "x" }, Var { name = "x" }))

let%test _ =
  eval
    (function "a" -> false | _ -> assert false)
    (Binop (Impl, Var { name = "a" }, Var { name = "b" }))

module _ = struct
  type bool2 = True | False
  type switch = On | Off
  type decision = Leave | Throw_away

  let switch_of_decision = function Leave -> On | Throw_away -> Off
  let decision_of_switch = function On -> Leave | Off -> Throw_away
  let () = assert (switch_of_decision (decision_of_switch On) = On)

  module _ = struct
    type postal = string
    type email = string
    type name = string

    (** note1 = name * postal + name * email *)
    type note1 = Postal of name * postal | Email of name * email

    (* address  = postal + email *)
    type address = Postal2 of postal | Email2 of email

    type note2 = name * address
    (** note2 = name * address *)

    let id (x : note1) = x

    let note2_of_note1 = function
      | Postal (name, post) -> (name, Postal2 post)
      | Email (name, post) -> (name, Email2 post)

    let note1_of_note2 = function
      | name, Postal2 p -> Postal (name, p)
      | name, Email2 email -> Email (name, email)
  end

  module _ = struct
    (* mylist(a) =  a  * mylist(a) + 1 *)
    type 'a mylist = Cons of 'a * 'a mylist | Nil

    let _ = Cons (1, Cons (1, Nil))

    (* list(a) = 1 + a  * list(a) *)
    type 'a list = 'a List.t = [] | ( :: ) of 'a * 'a list

    let _ = [ 1; 2 ]

    let rec mylist_of_list = function
      | [] -> Nil
      | h :: tl -> Cons (h, mylist_of_list tl)

    let rec list_of_mylist = function
      | Nil -> []
      | Cons (h, tl) -> h :: list_of_mylist tl
  end
end

(*
vector<string>  foo(vector<int> &arr) {
  vector<string> rez;
  foreach(int n in arr) {
    rez.push_back(std::string(n));
  }
  return rez
}
*)

module type TT = sig
  val zip : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
end

(* let _ = [ 1; "asdf" ] *)
let rec map : (* forall *) 'a 'b. ('a -> 'b) -> 'a list -> 'b list =
 fun f -> function [] -> [] | h :: tl -> f h :: map f tl

let () = assert (map (fun x -> x + 1) [ 1; 2; 3 ] = [ 2; 3; 4 ])

(*
int sum(vector<int> numbers) {
  int rez=0;
  for(int n in  numbers)
    rez += n;
  return rez;
}
*)

let rec fold f init = function [] -> init | h :: tl -> fold f (f init h) tl

let () =
  assert (fold ( + ) 0 [ 1; 2; 3; 4; 5 ] = 15);
  assert (List.fold_right ( + ) [ 1; 2; 3; 4; 5 ] 0 = 15)

module Option = struct
  let map f = function None -> None | Some x -> Some (f x)
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let map1 : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t = fun f (a, b) -> (f a, b)
  let map2 f (a, b) = (a, f b)
end

module _ = struct
  type 'a t = int -> 'a

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f cont n -> f (cont n)
end

module Map_is_not_Definable = struct
  type 'a t = 'a -> int
end
