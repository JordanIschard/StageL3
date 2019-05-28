open Tts.MachineTTS ;;
open String ;;
open Printf ;;
open List ;;
open LangISWIMTTS.ISWIM ;;

let expression1 = (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;
let expression2 = (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

start expression1 false ;;
Printf.printf "\n" ;;

start expression2 false;;
Printf.printf "\n" ;;