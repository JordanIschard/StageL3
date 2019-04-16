open SecdConc.SECDMachine ;;
open String ;;
open Printf ;;
open List ;;
open LangISWIMConc.ISWIM ;;

let expression4 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression5 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;
let expression6 = (
  Signal("signal",
    (App(
      (Spawn
        (Present("signal",expression5,Const 3))
      )
      ,(Spawn
        (Emit "signal"))
      )
    )
  )
) ;;

Printf.printf " Test de la MachineSECD\n\n" ;;

lancerSECD expression5 ;;
Printf.printf "\n" ;;

lancerSECD expression4 ;;
Printf.printf "\n" ;;

lancerSECD expression6 ;;
Printf.printf "\n" ;;