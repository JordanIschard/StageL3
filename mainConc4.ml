open SecdConc4.MachineTTSI ;;
open String ;;
open Printf ;;
open List ;;
open LangISWIMConc4.ISWIM ;;

let expression1 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression2 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

let expression3 = (App(Abs("x",(
                              App(
                                App(
                                  Spawn_ISWIM(Put_ISWIM("x", 4))
                                 ,Spawn_ISWIM(Put_ISWIM("x",3))
                                   )
                               ,Spawn_ISWIM(App(Wait,Get_ISWIM("x",1,0)))
                                  )
                                )),Signal_ISWIM));;

let expression4 = (App(
                        App(
                            Abs("s",
                                Abs("s1",
                                    App(
                                        App(
                                             Spawn_ISWIM(Present_ISWIM("s",Const 6,Const 9))
                                           , Spawn_ISWIM(Present_ISWIM("s1",Const 3,Const 5))
                                           )
                                       , Spawn_ISWIM(Put_ISWIM("s",0))
                                       )
                                   )
                               )
                          , Signal_ISWIM
                          )
                      , Signal_ISWIM
                      )
                  );;

start expression1 ;;
Printf.printf "\n" ;;

start expression2 ;;
Printf.printf "\n" ;;

start expression3 ;;
Printf.printf "\n" ;;

start expression4 ;;
Printf.printf "\n" ;;