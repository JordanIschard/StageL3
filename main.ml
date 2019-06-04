open Printf ;;
open Lambda.LambdaCalcul ;;
open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open Ck.CKMachine ;;
open Cek.CEKMachine ;;
open Secd.SECDMachine ;;
open Tts.MachineTTS;;
open Ttsi.MachineTTSI ;;


(* Test *)

(* Correspond à l'expression suivante : ( (lam w.( w ( + 2 w ) ))  2 ) *)
let expression  = let open LangISWIM.ISWIM in App((Abs("w",(App(Var "w",Op(Add,[Const 2;Var "w"]))))),Const 2) ;;

(* Correspond à l'expression suivante : ( (lam x.( x y )) z ) *)
let expression1 = let open LangISWIM.ISWIM in App(Abs("x",App(Var "x", Var "y" )),Var "z");;

(*  Correspond à l'expression suivante : ( (lam x.lam y.( x y )) (lam y.( y f )) ) *)
let expression2 = let open LangISWIM.ISWIM in App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f")));;

(*  Correspond à l'expression suivante : ( (lam z.lam b.( z b )) (lam a.( a g )) ) *)
let expression3 = let open LangISWIM.ISWIM in App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g")));;

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression4 = let open LangISWIM.ISWIM in App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"])))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression5 = let open LangISWIM.ISWIM in App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1);;

(*  Correspond à l'expression suivante : ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) *)
let expression6 = let open LangISWIM.ISWIM in App(Abs("x",App(Var "x",Var "x")), Abs("y",App(Abs("x",Var "x"),Abs("x",Var "x"))));;

(*  Correspond à l'expression suivante : ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) *)
let expression7 = let open LangISWIM.ISWIM in Op(Add4,[App(Abs("x",Var "x"),Const 2); App(Abs("x",Var "x"),Const 3); Const 4;App(Abs("x",Var "x"),Const 5)]);;

(* Correspond à l'expression suivante : ( (lam x.lam y.( x y )) (lam y.( y y )) ) *)
let expression8 = App_term(Abs_term("x",Abs_term("y",App_term(Var_term "x", Var_term "y"))),Abs_term("y",App_term(Var_term "y",Var_term "y")));;







(**** Partie lambda-calcul ****)

printf "Tests des Lambda-calculs \n\n" ;;

afficherTerme expression8 ;;
let resT = beta_reduction_term expression8 ;;
afficherTerme resT ;;
printf "\n" ;;





(**** Partie ISWIM ****)

printf "Tests du langage ISWIM \n\n" ;; 

printf "Réduction \n" ;;
let res4 = let open LangISWIM.ISWIM in n_red expression4 ;;
printf "\n" ;;


printf "Réduction \n" ;;
let res4 = let open LangISWIM.ISWIM in n_red expression6;;
printf "\n" ;;





(**** Partie Machine CC ****)

printf "Test de la MachineCC\n\n" ;;

lancerCC expression5 false;;
printf "\n" ;;

lancerCC expression4 false;;
printf "\n" ;;

lancerCC expression6 false;;
printf "\n" ;;

lancerCC expression7 true;;
printf "\n" ;;





(**** Partie Machine SCC ****)

printf "Test de la MachineSCC\n\n" ;;

lancerSCC expression5 false;;
printf "\n" ;;

lancerSCC expression4 false;;
printf "\n" ;;

lancerSCC expression6 false;;
printf "\n" ;;

lancerSCC expression7 false;;
printf "\n" ;;





(**** Partie Machine CK ****)

printf " Test de la MachineCK\n\n" ;;

lancerCK expression5 true;;
printf "\n" ;;

lancerCK expression4 false;;
printf "\n" ;;

lancerCK expression6 false;;
printf "\n" ;;

lancerCK expression7 false;;
printf "\n" ;;





(**** Partie Machine CEK ****)

printf " Test de la MachineCEK\n\n" ;;


lancerCEK expression4 false;;
printf "\n" ;;

lancerCEK expression5 false;;
printf "\n" ;;

lancerCEK expression6 false;;
printf "\n" ;;

lancerCK expression7 false;;
printf "\n" ;;





(**** Partie Machine SECD ****)

printf " Test de la MachineSECD\n\n" ;;

lancerSECD expression4 false;;
printf "\n" ;;


lancerSECD expression5 false;;
printf "\n" ;;

lancerSECD expression6 false;;
printf "\n" ;;

lancerSECD expression7 false;;
printf "\n" ;;





(**** Partie pour la machine SECD Concurrente V1 ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression9  = let open LangISWIMCv1.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression10 = let open LangISWIMCv1.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression10 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression11 = let open LangISWIMCv1.ISWIM in 
(Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression10;Signal_ISWIM("coucou",Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))));;





printf " Test de la MachineSECD concurrente version 1 \n\n" ;;

let open SecdCv1.SECDCv1Machine in lancerSECDCv1 expression9 false;;
printf "\n" ;;

let open SecdCv1.SECDCv1Machine in lancerSECDCv1 expression10 false;;
printf "\n" ;;

let open SecdCv1.SECDCv1Machine in lancerSECDCv1 expression11 false;;
printf "\n" ;;





(**** Partie pour la machine TTS ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression12 = let open LangISWIMTTS.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression13 = let open LangISWIMTTS.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(* Correspond à l'expression suivante : ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) *)
let expression14 = let open LangISWIMTTS.ISWIM in (App(App(Abs("s",Abs("s1",App(App(Spawn_ISWIM(Present_ISWIM("s",Const 6,Const 9)) , Spawn_ISWIM(Present_ISWIM("s1",Const 3,Const 5))), 
                                                  Spawn_ISWIM(Emit_ISWIM("s"))))), Signal_ISWIM), Signal_ISWIM));;





printf "Test de la MachineTTS\n\n" ;;

startTTS expression12 false ;;
printf "\n" ;;
                  
startTTS expression13 false;;
printf "\n" ;;
                  
startTTS expression14 false;;
printf "\n" ;;






(**** Partie pour la machine TTSI ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression15 = let open LangISWIMTTSI.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression16 = let open LangISWIMTTSI.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante : ( (lam x.( ( Spawn( put( x 4) ) Spawn( put( x 3 ) ) ) Spawn( wait get( x 1 0 ) ) )) init ) *)
let expression17 = let open LangISWIMTTSI.ISWIM in (App(Abs("x",(App(App(Spawn_ISWIM(Put_ISWIM("x", 4)),Spawn_ISWIM(Put_ISWIM("x",3))),Spawn_ISWIM(App(Wait,Get_ISWIM("x",1,0)))))),Signal_ISWIM));;





printf "Test de la MachineTTSI\n\n" ;;

startTTSI expression15 false ;;
printf "\n" ;;

startTTSI expression16 false;;
printf "\n" ;;

startTTSI expression17 false;;
printf "\n" ;;