open SecdConcE3.SECDMachine ;;
open String ;;
open Printf ;;
open List ;;
open LangISWIMConc3E.ISWIM ;;

let expression4 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression5 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;
let expression6 = (
  App (Signal_ISWIM "signal",
    (App(
      (Spawn
        (Present_ISWIM("signal",Op(Add,[expression5;App(Signal_ISWIM "coucou",Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))
      )
      ,(Spawn
        (Emit_ISWIM "signal"))
      )
    )
  )
) ;;

let expression7 = (Catch_ISWIM(12,App(
  Signal_ISWIM "signal",
    (App(
      (Spawn
        (Present_ISWIM("signal",Op(Add,[expression5;App (Signal_ISWIM "coucou",Present_ISWIM("usdhfozeih",Const 5,Const 65))]),Const 3))
      )
      ,(Spawn
        (Emit_ISWIM "signal"))
      )
    )
  )
,("uygig",Const 7685))) ;;
(*
let expression8 = Signal_ISWIM("salut",
                                    App(
                                       Spawn(
                                          Signal_ISWIM("coucou",
                                                Present_ISWIM("coucou",Const 98,App(Op(Sub,[Get_ISWIM("salut",2);Get_ISWIM("salut",2)]),Get_ISWIM("salut",2)))
                                                )
                                            )
                                      ,App(
                                         Spawn(
                                            App(
                                               Put_ISWIM("salut",6)
                                              ,App(Put_ISWIM("salut",3),Emit_ISWIM "salut")
                                              )
                                            )
                                        ,Spawn(
                                          Signal_ISWIM("coucou",
                                                  Present_ISWIM("coucou",Const 98,App(Op(Add,[Get_ISWIM("salut",2);Get_ISWIM("salut",2)]),Get_ISWIM("salut",2)))
                                                  )
                                             )
                                         )
                                      )
                              );;
*)
Printf.printf " Test de la MachineSECD\n\n" ;;

lancerSECD expression5 ;;
Printf.printf "\n" ;;

lancerSECD expression4 ;;
Printf.printf "\n" ;;
lancerSECD expression6 ;;
Printf.printf "\n" ;;

lancerSECD expression7 ;;
Printf.printf "\n" ;;
(*
lancerSECD expression8 ;;
Printf.printf "\n" ;;
*)
