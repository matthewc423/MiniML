(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v                                                                        
  | Unop (_, e) -> free_vars e                
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)   
  | Conditional (e1, e2, e3) ->  SS.union (SS.union (free_vars e1) 
                                       (free_vars e2)) (free_vars e3)
  | Fun (v, e) -> SS.remove v (free_vars e)                  
  | Let (v, e1, e2) -> SS.union (free_vars e1) (SS.remove v (free_vars e2))    
  | Letrec (v, e1, e2) -> SS.union (SS.remove v (free_vars e1)) 
                        (SS.remove v (free_vars e2))                            
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | _ -> SS.empty ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname =
   let ctr = ref 0 in 
   fun () ->
     let n = !ctr in 
     ctr := n + 1;
     string_of_int n ;;


(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr = 

  let rec subhelp (exp : expr) (fv : varidset) : expr =
    match exp with 
    | Var v -> if v = var_name then repl else exp 
    | Unop (u, e) -> Unop (u, subhelp e fv)
    | Binop (b, e1, e2) -> Binop (b, subhelp e1 fv, subhelp e2 fv)
    | Conditional (e1, e2, e3) -> Conditional (subhelp e1 fv, subhelp e2 fv, subhelp e3 fv)
    | Fun (v, e) -> if v = var_name then exp
                    else 
                      if SS.mem v fv then
                        let vnew = new_varname () in
                        Fun (vnew, subhelp (subst v (Var vnew) e) fv)
                      else
                        Fun(v, subhelp e fv)
    | Let (v, e1, e2) ->  if v = var_name then Let (v, subhelp e1 fv, e2)
                          else 
                            if SS.mem v fv then
                              let vnew = new_varname () in
                              Let (vnew, subhelp e1 fv, subhelp (subst v (Var vnew) e2) fv)
                            else Let (v, subhelp e1 fv, subhelp e2 fv)
    | Letrec (v, e1, e2) -> if v = var_name then exp
                            else
                              if SS.mem v fv then
                                let vnew = new_varname () in
                                Letrec (vnew, subhelp (subst v (Var vnew) e1) fv, subhelp (subst v (Var vnew) e2) fv)
                              else Letrec (v, subhelp e1 fv, subhelp e2 fv)
    | App (e1, e2) -> App (subhelp e1 fv, subhelp e2 fv)
    | _ -> exp in 

  subhelp exp (free_vars repl);;
  
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v -> "var " ^ v
  | Num n -> "num " ^ string_of_int n
  | Bool b -> "bool" ^ string_of_bool b
  | Unop (u, e) -> (match u with 
                   | Negate -> "negate " ^ exp_to_concrete_string e)
  | Binop (b, e1, e2) -> (match b with 
                         | Plus -> exp_to_concrete_string e1 ^ "plus " ^ exp_to_concrete_string e2
                         | Minus -> exp_to_concrete_string e1 ^ "minus " ^ exp_to_concrete_string e2
                         | Times -> exp_to_concrete_string e1 ^ "times " ^ exp_to_concrete_string e2
                         | Equals -> exp_to_concrete_string e1 ^ "equals " ^ exp_to_concrete_string e2
                         | LessThan -> exp_to_concrete_string e1 ^ "less than " ^ exp_to_concrete_string e2)
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_concrete_string e1 ^ "then " ^ exp_to_concrete_string e2 ^ "else " ^ exp_to_concrete_string e3
  | Fun (v, e) -> "fun " ^ v ^ exp_to_concrete_string e
  | Let (v, e1, e2) -> "let " ^ v ^ exp_to_concrete_string e1 ^ "be" ^ exp_to_concrete_string e2
  | Letrec (v, e1, e2) -> "let rec " ^ v ^ exp_to_concrete_string e1 ^ "be" ^ exp_to_concrete_string e2
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (e1, e2) -> "apply" ^ exp_to_concrete_string e1 ^ exp_to_concrete_string e2 ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v -> "Var(" ^ v ^ ")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (u, e) -> (match u with 
                   | Negate -> "Unop(~-, " ^ exp_to_abstract_string e ^ ")")
  
  | Binop (b, e1, e2) -> (match b with 
                         | Plus -> "Binop(+, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
                         | Minus -> "Binop(-, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
                         | Times -> "Binop(*, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
                         | Equals -> "Binop(=, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
                         | LessThan -> "Binop(<, " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")")
  | Conditional (e1, e2, e3) -> "Conditional(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (v, e1, e2) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (v, e1, e2) -> "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")" ;;