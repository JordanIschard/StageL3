open Lambda.LambdaCalcul ;;
open LangISWIM.ISWIM;;
open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open Ck.CKMachine ;;
open Cek.CEKMachine ;;
open Secd.SECDMachine ;;


(* Test *)

Printf.printf "Tests des Lambda-calculs \n\n" ;;

let terme = (App_term(Abs_term("x",Abs_term("y",App_term(Var_term "x", Var_term "y"))),Abs_term("y",App_term(Var_term "y",Var_term "y"))));;

afficherTerme terme ;;
let resT = beta_reduction_term terme;;
afficherTerme resT ;;
Printf.printf "\n" ;;

Printf.printf "Tests du langage ISWIM \n\n" ;; 


let expression = (App((Abs("w",(App(Var "w",Op(Add,[Const 2;Var "w"]))))),Const 2)) ;;
let expression1 = (App(Abs("x",App(Var "x", Var "y" )),Var "z"));;
let expression2 = (App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f"))));;
let expression3 = (App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g"))));;
let expression4 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression5 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;
let expression6 = (App(
                        Abs("x",App(Var "x",Var "x"))
                      , Abs("y",App(
                                     Abs("x",Var "x")
                                    ,Abs("x",Var "x")
                                   )
                           )
                      )
                  );;

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


Printf.printf "Réduction \n" ;;
let res4 = n_red expression6;;
Printf.printf "\n" ;;

Printf.printf "Test de la MachineCC\n\n" ;;

lancerCC expression5 ;;
Printf.printf "\n" ;;

lancerCC expression4 ;;
Printf.printf "\n" ;;

lancerCC expression6 ;;
Printf.printf "\n" ;;

(*

Printf.printf "Test de la MachineSCC\n\n" ;;

lancerSCC expression5 ;;
Printf.printf "\n" ;;

lancerSCC expression4 ;;
Printf.printf "\n" ;;

lancerSCC expression6 ;;
Printf.printf "\n" ;;


Printf.printf " Test de la MachineCK\n\n" ;;

let registreCK  = Fun (Abs("x",Var "x"),Arg((Var "y"),(Opd(([(Const 1);(Var "z")],Add),[],MT)))) ;;

afficherCK (Var "coucou") registreCK ;;
Printf.printf "\n" ;;

lancerCK expression5 ;;
Printf.printf "\n" ;;

lancerCK expression4 ;;
Printf.printf "\n" ;;

lancerCK expression6 ;;
Printf.printf "\n" ;;


Printf.printf " Test de la MachineCEK\n\n" ;;

let registreCEK  = Fun_CEK ((Fermeture((Abs("x",Var "x")),[])),MT_CEK) ;;
let expressionCEK = Fermeture(Const 1,[]) ;;
let clause = (Fermeture(Abs("x",Var "x"),[])) ;;

Printf.printf "%s\n" (string_of_fermeture clause) ;;

afficherCEK expressionCEK registreCEK ;;
Printf.printf "\n" ;;

lancerCEK expression5 ;;
Printf.printf "\n" ;;

lancerCEK expression4 ;;
Printf.printf "\n" ;;

lancerCEK expression6 ;;
Printf.printf "\n" ;;

Printf.printf " Test de la MachineSECD\n\n" ;;

afficherExpr expression5 ;;
let secd = MachineSECD([],[],(secdLanguage_of_exprISWIM expression5),Vide_D) ;;
afficherSECD secd ;;

lancerSECD expression5 ;;
Printf.printf "\n" ;;


lancerSECD expression4 ;;
Printf.printf "\n" ;;

lancerSECD expression6 ;;
Printf.printf "\n" ;;*)