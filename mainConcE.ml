open SecdConcE.SECDMachine ;;
open String ;;
open Printf ;;
open List ;;
open LangISWIMConcE.ISWIM ;;

let expression4 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression5 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;
let expression6 = (
  Signal("signal",
    (App(
      (Spawn
        (Present("signal",Op(Add,[expression5;Signal("coucou",Present("coucou",Const 5,Const 65))]),Const 3))
      )
      ,(Spawn
        (Emit "signal"))
      )
    )
  )
) ;;
let expression7 = (Catch(7,
  Signal("signal",
    (App(
      (Spawn
        (Present("signal",Op(Add,[expression5;Signal("coucou",Present("usdhfozeih",Const 5,Const 65))]),Const 3))
      )
      ,(Spawn
        (Emit "signal"))
      )
    )
  )
,("uygig",Const 7685))) ;;


Printf.printf " Test de la MachineSECD\n\n" ;;

lancerSECD expression5 ;;
Printf.printf "\n" ;;

lancerSECD expression4 ;;
Printf.printf "\n" ;;

lancerSECD expression6 ;;
Printf.printf "\n" ;;

lancerSECD expression7 ;;
Printf.printf "\n" ;;