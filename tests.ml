
module Ev = Evaluation ;;
module MP = Miniml_parse ;;
module ML = Miniml_lex ;;
module Ex = Expr ;;

let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp ;;

let unit_test (condition : bool) (msg : string) : unit =  
  if condition then
    Printf.printf "%s passed\n" msg
  else
    Printf.printf "%s FAILED\n" msg ;;

let free_vars_test () =
  unit_test (Ex.same_vars (Ex.vars_of_list ["x"]) (Ex.free_vars (str_to_exp "x ;;")))
            "free_vars x";
  unit_test (Ex.same_vars (Ex.vars_of_list ["x"]) (Ex.free_vars (str_to_exp "x * x ;;")))
            "free_vars x * x";
  unit_test (Ex.same_vars (Ex.vars_of_list ["x"]) (Ex.free_vars (str_to_exp "let y = x in y ;;")))
            "free_vars let y = x in y";
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (str_to_exp "let rec x = fun y -> x in x ;;")))
            "free_vars let rec x = fun y -> x in x";
  unit_test (Ex.same_vars (Ex.vars_of_list ["f"]) (Ex.free_vars (str_to_exp "let x = 2 in f x ;;")))
            "free_vars let x = 2 in f x";
  unit_test (Ex.same_vars (Ex.vars_of_list ["x"]) (Ex.free_vars (str_to_exp "if x = 2 then x else 3 ;;")))
            "free_vars if x = 2 then x else 3";;

let subst_test () =
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (Ex.subst "x" Ex.(Num 3) (str_to_exp "x ;;"))))
            "subst x";
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (Ex.subst "x" Ex.(Num 3) (str_to_exp "x * x ;;"))))
            "subst x * x";
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (Ex.subst "x" Ex.(Num 3) (str_to_exp "let y = x in y ;;"))))
            "subst let y = x in y";
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (Ex.subst "x" Ex.(Num 3) (str_to_exp "let rec x = fun y -> x in x ;;"))))
            "subst let rec x = fun y -> x in x";
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (Ex.subst "f" Ex.(Fun ("y", Var "y")) (str_to_exp "let x = 2 in f x ;;"))))
            "subst let x = 2 in f x";
  unit_test (Ex.same_vars (Ex.vars_of_list []) (Ex.free_vars (Ex.subst "x" Ex.(Num 3) (str_to_exp "if x = 2 then x else 3 ;;"))))
            "subst if x = 2 then x else 3";;

let env_test () =
  unit_test (Ev.Env.env_to_string (Ev.Env.empty ()) = "")
            "Env.empty ()";
  unit_test (Ev.Env.env_to_string (Ev.Env.extend (Ev.Env.empty ()) "x" (ref (Ev.Env.Val Ex.(Num 1)))) = "(x, Val Num(1))")
            "Env.extend x (Num 1)";
  unit_test (Ev.Env.env_to_string (Ev.Env.extend (Ev.Env.extend (Ev.Env.empty ()) "x" (ref (Ev.Env.Val Ex.(Num 1)))) "x" (ref (Ev.Env.Val Ex.(Num 2)))) = "(x, Val Num(2))")
            "Env.extend x (Num 1) and then x (Num 2)";
  unit_test (Ev.Env.env_to_string (Ev.Env.extend (Ev.Env.extend (Ev.Env.empty ()) "x" (ref (Ev.Env.Val Ex.(Num 1)))) "y" (ref (Ev.Env.Val Ex.(Num 2)))) = "(y, Val Num(2))(x, Val Num(1))")
            "Env.extend x (Num 1) and then y (Num 2)";
  unit_test (Ev.Env.lookup (Ev.Env.extend (Ev.Env.empty ()) "x" (ref (Ev.Env.Val Ex.(Num 1)))) "x" = Ev.Env.Val Ex.(Num(1)))
            "Env.lookup env x";           
  unit_test (Ev.Env.lookup (Ev.Env.extend (Ev.Env.extend (Ev.Env.empty ()) "x" (ref (Ev.Env.Val Ex.(Num 1)))) "y" (ref (Ev.Env.Val Ex.(Num 2)))) "y" = Ev.Env.Val Ex.(Num(2)))
            "Env.lookup env y";
  unit_test (Ev.Env.value_to_string (Ev.Env.close Ex.(Fun ("y", Ex.Binop (Ex.Plus, Ex.Var("x"), Ex.Var("y")))) (Ev.Env.extend (Ev.Env.empty ()) "x" (ref (Ev.Env.Val Ex.(Num 1))))) = "Closure(Fun(y, Binop(+, Var(x), Var(y))), (x, Val Num(1)))")
            "Env.close exp env";;

let eval_s_test () = 
  unit_test (Ev.eval_s (str_to_exp "3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_s 3";
  unit_test (Ev.eval_s (str_to_exp "3 + 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 7))
            "eval_s 3 + 4";
  unit_test (Ev.eval_s (str_to_exp "3 - 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num ~-1))
            "eval_s 3 - 4";
  unit_test (Ev.eval_s (str_to_exp "3 * 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 12))
            "eval_s 3 * 4";
  unit_test (Ev.eval_s (str_to_exp "3 = 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool false))
            "eval_s 3 = 4";
  unit_test (Ev.eval_s (str_to_exp "3 = 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool true))
            "eval_s 3 = 3";
  unit_test (Ev.eval_s (str_to_exp "3 < 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool true))
            "eval_s 3 < 4";
  unit_test (Ev.eval_s (str_to_exp "4 < 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool false))
            "eval_s 4 < 3";
  unit_test (Ev.eval_s (str_to_exp "~-1 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num ~-1))
            "eval_s ~-1";
  unit_test (Ev.eval_s (str_to_exp "if 2 = 3 then 1 else 2 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 2))
            "eval_s if 2 = 3 then 1 else 2";
  unit_test (Ev.eval_s (str_to_exp "if 3 = 3 then 1 else 2 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 1))
            "eval_s if 3 = 3 then 1 else 2";
  unit_test (Ev.eval_s (str_to_exp "let x = 2 in x ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 2))
            "eval_s let x = 2 in x";
  unit_test (Ev.eval_s (str_to_exp "let x = 2 in let y = 3 in x * y ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 6))
            "eval_s let x = 2 in let y = 3 in x * y";
  unit_test (Ev.eval_s (str_to_exp "let f = fun x -> x in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_s let f = fun x -> x in f 3";
  unit_test (Ev.eval_s (str_to_exp "let f = fun x -> x in f f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_s let f = fun x -> x in f f 3";
  unit_test (Ev.eval_s (str_to_exp "let f = fun x -> x * 5 in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 15))
            "eval_s let f = fun x -> x * 5 in f 3";
  unit_test (Ev.eval_s (str_to_exp "let rec f = fun x -> if x = 0 then x else x + f (x - 1) in f 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 10))
            "eval_s let rec f = fun x -> if x = 0 then x else x + f (x - 1) in f 4";
  unit_test (Ev.eval_s (str_to_exp "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 24))
            "eval_s let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4";;

let eval_d_test () = 
  unit_test (Ev.eval_d (str_to_exp "3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_d 3";
  unit_test (Ev.eval_d (str_to_exp "3 + 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 7))
            "eval_d 3 + 4";
  unit_test (Ev.eval_d (str_to_exp "3 - 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num ~-1))
            "eval_d 3 - 4";
  unit_test (Ev.eval_d (str_to_exp "3 * 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 12))
            "eval_d 3 * 4";
  unit_test (Ev.eval_d (str_to_exp "3 = 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool false))
            "eval_d 3 = 4";
  unit_test (Ev.eval_d (str_to_exp "3 = 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool true))
            "eval_d 3 = 3";
  unit_test (Ev.eval_d (str_to_exp "3 < 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool true))
            "eval_d 3 < 4";
  unit_test (Ev.eval_d (str_to_exp "4 < 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool false))
            "eval_d 4 < 3";
  unit_test (Ev.eval_d (str_to_exp "~-1 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num ~-1))
            "eval_d ~-1";
  unit_test (Ev.eval_d (str_to_exp "if 2 = 3 then 1 else 2 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 2))
            "eval_d if 2 = 3 then 1 else 2";
  unit_test (Ev.eval_d (str_to_exp "if 3 = 3 then 1 else 2 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 1))
            "eval_d if 3 = 3 then 1 else 2";
  unit_test (Ev.eval_d (str_to_exp "let x = 2 in x ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 2))
            "eval_d let x = 2 in x";
  unit_test (Ev.eval_d (str_to_exp "let x = 2 in let y = 3 in x * y ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 6))
            "eval_d let x = 2 in let y = 3 in x * y";
  unit_test (Ev.eval_d (str_to_exp "let f = fun x -> x in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_d let f = fun x -> x in f 3";
  unit_test (Ev.eval_d (str_to_exp "let f = fun x -> x in f f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_d let f = fun x -> x in f f 3";
  unit_test (Ev.eval_d (str_to_exp "let f = fun x -> x * 5 in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 15))
            "eval_d let f = fun x -> x * 5 in f 3";
  unit_test (Ev.eval_d (str_to_exp "let rec f = fun x -> if x = 0 then x else x + f (x - 1) in f 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 10))
            "eval_d let rec f = fun x -> if x = 0 then x else x + f (x - 1) in f 4";
  unit_test (Ev.eval_d (str_to_exp "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 24))
            "eval_d let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4";
  unit_test (Ev.eval_d (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 5))
            "eval_d let x = 1 in let f = fun y -> x + y in let x = 2 in f 3";;

let eval_l_test () = 
  unit_test (Ev.eval_l (str_to_exp "3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_l 3";
  unit_test (Ev.eval_l (str_to_exp "3 + 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 7))
            "eval_l 3 + 4";
  unit_test (Ev.eval_l (str_to_exp "3 - 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num ~-1))
            "eval_l 3 - 4";
  unit_test (Ev.eval_l (str_to_exp "3 * 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 12))
            "eval_l 3 * 4";
  unit_test (Ev.eval_l (str_to_exp "3 = 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool false))
            "eval_l 3 = 4";
  unit_test (Ev.eval_l (str_to_exp "3 = 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool true))
            "eval_l 3 = 3";
  unit_test (Ev.eval_l (str_to_exp "3 < 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool true))
            "eval_l 3 < 4";
  unit_test (Ev.eval_l (str_to_exp "4 < 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Bool false))
            "eval_l 4 < 3";
  unit_test (Ev.eval_l (str_to_exp "~-1 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num ~-1))
            "eval_l ~-1";
  unit_test (Ev.eval_l (str_to_exp "if 2 = 3 then 1 else 2 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 2))
            "eval_l if 2 = 3 then 1 else 2";
  unit_test (Ev.eval_l (str_to_exp "if 3 = 3 then 1 else 2 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 1))
            "eval_l if 3 = 3 then 1 else 2";
  unit_test (Ev.eval_l (str_to_exp "let x = 2 in x ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 2))
            "eval_l let x = 2 in x";
  unit_test (Ev.eval_l (str_to_exp "let x = 2 in let y = 3 in x * y ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 6))
            "eval_l let x = 2 in let y = 3 in x * y";
  unit_test (Ev.eval_l (str_to_exp "let f = fun x -> x in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_l let f = fun x -> x in f 3";
  unit_test (Ev.eval_l (str_to_exp "let f = fun x -> x in f f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 3))
            "eval_l let f = fun x -> x in f f 3";
  unit_test (Ev.eval_l (str_to_exp "let f = fun x -> x * 5 in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 15))
            "eval_l let f = fun x -> x * 5 in f 3";
  unit_test (Ev.eval_l (str_to_exp "let rec f = fun x -> if x = 0 then x else x + f (x - 1) in f 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 10))
            "eval_l let rec f = fun x -> if x = 0 then x else x + f (x - 1) in f 4";
  unit_test (Ev.eval_l (str_to_exp "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 24))
            "eval_l let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4";
  unit_test (Ev.eval_l (str_to_exp "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;") (Ev.Env.empty ()) = Ev.Env.Val Ex.(Num 4))
            "eval_l let x = 1 in let f = fun y -> x + y in let x = 2 in f 3" ;;

let test () =
  free_vars_test () ;
  subst_test () ;
  env_test () ;
  eval_s_test () ;
  eval_d_test () ;
  eval_l_test () ;;

let _ = test () ;;