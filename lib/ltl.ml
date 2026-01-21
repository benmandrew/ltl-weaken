open Core

(* type prop_formula =
  | True
  | False
  | Atom of string
  | Not of prop_formula
  | And of prop_formula list
  | Or of prop_formula list *)

(* Phantom type markers *)
type prop_kind
type temporal_kind

type _ formula =
  (* Propositional operators - work on any formula *)
  | PTrue : 'k formula
  | PFalse : 'k formula
  | PAtom : string -> 'k formula
  | PNot : 'k formula -> 'k formula
  | PAnd : 'k formula list -> 'k formula
  | POr : 'k formula list -> 'k formula
  | PImply : 'k formula * 'k formula -> 'k formula
  | PIff : 'k formula * 'k formula -> 'k formula
  (* Temporal operators - only on temporal formulas *)
  | Next : _ formula -> temporal_kind formula
  | Until : _ formula * _ formula -> temporal_kind formula
  | Release : _ formula * _ formula -> temporal_kind formula
  | Globally : _ formula -> temporal_kind formula
  | Finally : _ formula -> temporal_kind formula

type t = temporal_kind formula
type prop = prop_kind formula

let rec to_string : type a. a formula -> string = function
  | PTrue -> "TRUE"
  | PFalse -> "FALSE"
  | PAtom s -> s
  | PNot f -> Printf.sprintf "(!%s)" (to_string f)
  | PAnd fs ->
      Printf.sprintf "(%s)"
        (String.concat ~sep:" & " (List.map fs ~f:to_string))
  | POr fs ->
      Printf.sprintf "(%s)"
        (String.concat ~sep:" | " (List.map fs ~f:to_string))
  | PImply (f1, f2) -> Printf.sprintf "(%s -> %s)" (to_string f1) (to_string f2)
  | PIff (f1, f2) -> Printf.sprintf "(%s <-> %s)" (to_string f1) (to_string f2)
  | Next f -> Printf.sprintf "(X %s)" (to_string f)
  | Until (f1, f2) -> Printf.sprintf "(%s U %s)" (to_string f1) (to_string f2)
  | Release (f1, f2) -> Printf.sprintf "(%s R %s)" (to_string f1) (to_string f2)
  | Globally f -> Printf.sprintf "(G %s)" (to_string f)
  | Finally f -> Printf.sprintf "(F %s)" (to_string f)

(* let rec prop_to_string : prop_formula -> string = function
  | True -> "true"
  | False -> "false"
  | Atom s -> s
  | Not f -> Printf.sprintf "(!%s)" (prop_to_string f)
  | And fs -> Printf.sprintf "(%s)" (String.concat ~sep:" & " (List.map fs ~f:prop_to_string))
  | Or fs -> Printf.sprintf "(%s)" (String.concat ~sep:" | " (List.map fs ~f:prop_to_string)) *)

type any_formula = Any : 'k formula -> any_formula

let rec index : type a. a formula -> int list -> any_formula =
 fun t -> function
  | [] -> Any t
  | i :: is -> begin
      match t with
      | PNot f -> index f is
      | PAnd fs -> index (List.nth_exn fs i) is
      | POr fs -> index (List.nth_exn fs i) is
      | PImply (f1, f2) -> if i = 0 then index f1 is else index f2 is
      | PIff (f1, f2) -> if i = 0 then index f1 is else index f2 is
      | Next f -> index f is
      | Until (f1, f2) -> if i = 0 then index f1 is else index f2 is
      | Release (f1, f2) -> if i = 0 then index f1 is else index f2 is
      | Globally f -> index f is
      | Finally f -> index f is
      | f -> failwith (Printf.sprintf "Cannot index into %s" (to_string f))
    end

let rec compare_formula : type k1 k2. k1 formula -> k2 formula -> int =
 fun a b ->
  match (a, b) with
  | PTrue, PTrue -> 0
  | PTrue, _ -> -1
  | _, PTrue -> 1
  | PFalse, PFalse -> 0
  | PFalse, _ -> -1
  | _, PFalse -> 1
  | PAtom x, PAtom y -> String.compare x y
  | PAtom _, _ -> -1
  | _, PAtom _ -> 1
  | PNot x, PNot y -> compare_formula x y
  | PNot _, _ -> -1
  | _, PNot _ -> 1
  | PAnd xs, PAnd ys ->
      List.compare compare_any
        (List.map xs ~f:(fun x -> Any x))
        (List.map ys ~f:(fun y -> Any y))
  | PAnd _, _ -> -1
  | _, PAnd _ -> 1
  | POr xs, POr ys ->
      List.compare compare_any
        (List.map xs ~f:(fun x -> Any x))
        (List.map ys ~f:(fun y -> Any y))
  | POr _, _ -> -1
  | _, POr _ -> 1
  | PImply (a1, b1), PImply (a2, b2) ->
      let c = compare_formula a1 a2 in
      if c <> 0 then c else compare_formula b1 b2
  | PImply _, _ -> -1
  | _, PImply _ -> 1
  | PIff (a1, b1), PIff (a2, b2) ->
      let c = compare_formula a1 a2 in
      if c <> 0 then c else compare_formula b1 b2
  | PIff _, _ -> -1
  | _, PIff _ -> 1
  | Next x, Next y -> compare_formula x y
  | Next _, _ -> -1
  | _, Next _ -> 1
  | Until (a1, b1), Until (a2, b2) ->
      let c = compare_formula a1 a2 in
      if c <> 0 then c else compare_formula b1 b2
  | Until _, _ -> -1
  | _, Until _ -> 1
  | Release (a1, b1), Release (a2, b2) ->
      let c = compare_formula a1 a2 in
      if c <> 0 then c else compare_formula b1 b2
  | Release _, _ -> -1
  | _, Release _ -> 1
  | Globally x, Globally y -> compare_formula x y
  | Globally _, _ -> -1
  | _, Globally _ -> 1
  | Finally x, Finally y -> compare_formula x y

and compare_any (Any a) (Any b) = compare_formula a b

let rec hash_fold_formula : type k. Hash.state -> k formula -> Hash.state =
 fun st -> function
  | PTrue -> Hash.fold_int st 0
  | PFalse -> Hash.fold_int st 1
  | PAtom s -> Hash.fold_int (Hash.fold_string st s) 2
  | PNot f -> Hash.fold_int (hash_fold_formula st f) 3
  | PAnd fs ->
      let st' = List.fold fs ~init:st ~f:hash_fold_formula in
      Hash.fold_int st' (-4)
  | POr fs ->
      let st' = List.fold fs ~init:st ~f:hash_fold_formula in
      Hash.fold_int st' (-5)
  | PImply (a, b) ->
      Hash.fold_int (hash_fold_formula (hash_fold_formula st a) b) 6
  | PIff (a, b) ->
      Hash.fold_int (hash_fold_formula (hash_fold_formula st a) b) 7
  | Next f -> Hash.fold_int (hash_fold_formula st f) 8
  | Until (a, b) ->
      Hash.fold_int (hash_fold_formula (hash_fold_formula st a) b) 9
  | Release (a, b) ->
      Hash.fold_int (hash_fold_formula (hash_fold_formula st a) b) 10
  | Globally f -> Hash.fold_int (hash_fold_formula st f) 11
  | Finally f -> Hash.fold_int (hash_fold_formula st f) 12

let hash_formula f = Hash.get_hash_value (hash_fold_formula (Hash.create ()) f)

module Any_formula = struct
  module T = struct
    type t = any_formula

    let hash_fold_t st (Any f) = hash_fold_formula st f
    let compare (Any a) (Any b) = compare_formula a b
    let hash (Any a) = hash_formula a
    let sexp_of_t (Any f) = Sexp.Atom (to_string f)
  end

  include T
  include Hashable.Make_plain (T)
end
