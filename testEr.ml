open LangISWIME.ISWIM ;;

let expression = (App((Abs("w",(App(Var "w",Op(Add,[Const 2;Var "w"]))))),Const 2)) ;;
let expression1 = (App(Abs("x",App(Var "x", Var "y" )),Var "z"));;
let expression2 = (App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f"))));;
let expression3 = (App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g"))));;
let expression4 = (App(Abs("w",Op(Sub,[Var "w"])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Op(Add,[Var "y"])))));;
let expression5 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;


n_red expression ;;
Printf.printf "\n" ;;
Printf.printf "%b\n\n" (equalExpr (Erreur "hooho") (Erreur "newExpr")) ;;

n_red expression1 ;;
Printf.printf "\n\n" ;;

n_red expression2 ;;
Printf.printf "\n\n" ;;

n_red expression3 ;;
Printf.printf "\n\n" ;;

n_red expression4 ;;
Printf.printf "\n\n" ;;

n_red expression5 ;;
Printf.printf "\n\n" ;;