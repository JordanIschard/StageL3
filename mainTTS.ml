open Tts.MachineTTS ;;
open String ;;
open Printf ;;
open List ;;
open LangISWIMTTS.ISWIM ;;

let expression1 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression2 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

let expression3 = (App(
                        App(
                            Abs("s",
                                Abs("s1",
                                    App(
                                        App(
                                             Spawn_ISWIM(Present_ISWIM("s",Const 6,Const 9))
                                           , Spawn_ISWIM(Present_ISWIM("s1",Const 3,Const 5))
                                           )
                                       , Spawn_ISWIM(Emit_ISWIM("s"))
                                       )
                                   )
                               )
                          , Signal_ISWIM
                          )
                      , Signal_ISWIM
                      )
                  );;

start expression1 false ;;
Printf.printf "\n" ;;

start expression2 false;;
Printf.printf "\n" ;;

start expression3 true;;
Printf.printf "\n" ;;