open Lambda.LambdaCalcul ;;
open LangISWIM.ISWIM;;
open Cc.CCMachine ;;
open Scc.SCCMachine ;;


(* Test *)

let terme = (App_term(Abs_term("x",Abs_term("y",App_term(Var_term "x", Var_term "y"))),Abs_term("y",App_term(Var_term "y",Var_term "y"))));;

let expression = (App((Abs("w",(App(Var "w",Op("+",[Const 2;Var "w"]))))),Const 2)) ;;
let expression1 = (App(Abs("x",App(Var "x", Var "y" )),Var "z"));;
let expression2 = (App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f"))));;
let expression3 = (App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g"))));;
let expression4 = (App(Abs("w",Op("-",[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op("+",[Var "z";Var "y"]))))));;
let expression5 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op("+",[Var "y";Var "y"]))),Const 1));;

Printf.printf "Tests des Lambda-calculs \n\n" ;;

afficherTerme terme ;;
let resT = beta_reduction_term terme;;
afficherTerme resT ;;
Printf.printf "\n" ;;

Printf.printf "Tests du langage ISWIM \n\n" ;;

afficherExpr expression ;;
let res = beta_red expression ;;
afficherExpr res ;;
Printf.printf "\n" ;;

let resD = delta_red res ;;
afficherExpr resD ;;
Printf.printf "\n" ;;

afficherExpr expression1 ;;
let res1 = beta_red expression1 ;;
afficherExpr res1 ;;
Printf.printf "\n" ;;

afficherExpr expression2 ;;
let res2 = beta_red expression2 ;;
afficherExpr res2 ;;
Printf.printf "\n" ;;

afficherPairList (pre_alpha_eq expression2 expression3) ;;
Printf.printf "%b\n" (equalExpr expression2 expression3);;
Printf.printf "\n" ;;

Printf.printf "Réduction \n" ;;
let res4 = n_red expression4 ;;
Printf.printf "\n" ;;

Printf.printf "Test de la MachineCC\n\n" ;;

lancerCC expression5 ;;
Printf.printf "\n" ;;

lancerCC expression4 ;;
Printf.printf "\n" ;;