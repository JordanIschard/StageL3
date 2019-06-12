open Printf ;;
open Lambda.LambdaCalcul ;;
open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open Ck.CKMachine ;;
open Cek.CEKMachine ;;
open Secd.SECDMachine ;;
open SecdCv1.SECDCv1Machine;;
open SecdCv2.SECDCv2Machine;;
open SecdCv3.SECDCv3Machine;;
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

(* Correspond à l'expression suivante : ( (lam w.( w ( + 2 w ) )) 2 ) *)
let expression9  = let open LangISWIME.ISWIM in (App((Abs("w",(App(Var "w",Op(Add,[Const 2;Var "w"]))))),Const 2)) ;;

(* Correspond à l'expression suivante : ( (lam x.( x  y )) z ) *)
let expression10 = let open LangISWIME.ISWIM in (App(Abs("x",App(Var "x", Var "y" )),Var "z"));;

(* Correspond à l'expression suivante : ( (lam x.(lam y.( x y ))) (lam y.( y f )) ) *)
let expression11 = let open LangISWIME.ISWIM in (App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f"))));;

(* Correspond à l'expression suivante : ( (lam z.(lam b.( z b ))) (lam a.( a g )) ) *)
let expression12 = let open LangISWIME.ISWIM in (App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g"))));;

(* Correspond à l'expression suivante : ( (lam w.( - w ))  ( ( (lam x.( x 10 )) (lam y.( + y )) ) ) *)
let expression13 = let open LangISWIME.ISWIM in (App(Abs("w",Op(Sub,[Var "w"])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Op(Add,[Var "y"])))));;

(* Correspond à l'expression suivante : ( ( (lam f.(lam x.( f x ))) (lam y.( + y y )) ) 1 ) *)
let expression14 = let open LangISWIME.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;






(**** Partie lambda-calcul ****)

printf "\n\n\n\n\nTest des Lambda-calculs \n\n" ;;

afficherTerme expression8 ;;
let resT = beta_reduction_term expression8 ;;
afficherTerme resT ;;
printf "\n" ;;





(**** Partie ISWIM ****)

printf "\n\n\n\n\nTest du langage ISWIM \n\n" ;; 

printf "Réduction pour ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
let open LangISWIM.ISWIM in n_red expression4 ;;
printf "\n" ;;


printf "Réduction pour ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
let open LangISWIM.ISWIM in n_red expression6;;
printf "\n" ;;





(**** Partie ISWIM avec erreur ****)

printf "\n\n\n\n\nTest du langage ISWIM avec erreur \n\n" ;; 

printf "Réduction pour  ( (lam w.( w ( + 2 w ) )) 2 ) \n" ;;
let open LangISWIME.ISWIM in n_red expression9 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam x.( x  y )) z ) \n" ;;
let open LangISWIME.ISWIM in n_red expression10 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam x.(lam y.( x y ))) (lam y.( y f )) ) \n" ;;
let open LangISWIME.ISWIM in n_red expression11 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam z.(lam b.( z b ))) (lam a.( a g )) ) \n" ;;
let open LangISWIME.ISWIM in n_red expression12 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam w.( - w ))  ( ( (lam x.( x 10 )) (lam y.( + y )) ) ) \n" ;;
let open LangISWIME.ISWIM in n_red expression13 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( ( (lam f.(lam x.( f x ))) (lam y.( + y y )) ) 1 ) \n" ;;
let open LangISWIME.ISWIM in n_red expression14 ;;
Printf.printf "\n\n" ;;



(**** Partie Machine CC ****)

printf "\n\n\n\n\nTest de la MachineCC\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerCC expression4 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerCC expression5 false;;
printf "\n" ;;

printf "On teste l'expression  ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
lancerCC expression6 false;;
printf "\n" ;;

printf "On teste l'expression ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) \n" ;;
lancerCC expression7 true;;
printf "\n" ;;





(**** Partie Machine SCC ****)

printf "\n\n\n\n\nTest de la MachineSCC\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSCC expression4 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSCC expression5 false;;
printf "\n" ;;

printf "On teste l'expression  ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
lancerSCC expression6 false;;
printf "\n" ;;

printf "On teste l'expression ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) \n" ;;
lancerSCC expression7 false;;
printf "\n" ;;





(**** Partie Machine CK ****)

printf "\n\n\n\n\nTest de la MachineCK\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerCK expression4 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerCK expression5 true;;
printf "\n" ;;

printf "On teste l'expression  ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
lancerCK expression6 false;;
printf "\n" ;;

printf "On teste l'expression ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) \n" ;;
lancerCK expression7 false;;
printf "\n" ;;





(**** Partie Machine CEK ****)

printf "\n\n\n\n\nTest de la MachineCEK\n\n" ;;


printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerCEK expression4 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerCEK expression5 false;;
printf "\n" ;;

printf "On teste l'expression  ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
lancerCEK expression6 false;;
printf "\n" ;;

printf "On teste l'expression ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) \n" ;;
lancerCEK expression7 false;;
printf "\n" ;;





(**** Partie Machine SECD ****)

printf "\n\n\n\n\nTest de la MachineSECD\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECD expression4 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECD expression5 false;;
printf "\n" ;;

printf "On teste l'expression  ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
lancerSECD expression6 false;;
printf "\n" ;;

printf "On teste l'expression ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) \n" ;;
lancerSECD expression7 false;;
printf "\n" ;;





(**** Partie pour la machine SECD Concurrente V1 ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression15 = let open LangISWIMCv1.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression16 = let open LangISWIMCv1.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression15 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression17 = let open LangISWIMCv1.ISWIM in 
(Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression15;Signal_ISWIM("coucou",Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))));;





printf "\n\n\n\n\nTest de la MachineSECD concurrente version 1 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv1 expression15 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv1 expression16 false;;
printf "\n" ;;

printf "On teste l'expression  Signal( signal , ( Spawn( Present( signal , ( + expression15 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) \n" ;;
lancerSECDCv1 expression17 false;;
printf "\n" ;;





(**** Partie pour la machine SECD Concurrente V2 ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression18 = let open LangISWIMCv2.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression19 = let open LangISWIMCv2.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression18 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression20 = let open LangISWIMCv2.ISWIM in (Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression18;Signal_ISWIM("coucou",
Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))));;

(*  Correspond à l'expression suivante :  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression18 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) *)
let expression21 = let open LangISWIMCv2.ISWIM in (Catch_ISWIM(6,Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression18;
Signal_ISWIM("coucou",Present_ISWIM("usdhfozeih",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))),("uygig",Const 7685))) ;;





Printf.printf "\n\n\n\n\nTest de la Machine SECD concurrente version 2 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv2 expression18 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv2 expression19 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  Signal( signal , ( Spawn( Present( signal , ( + expression13 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) \n" ;;
lancerSECDCv2 expression20 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression13 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) \n" ;;
lancerSECDCv2 expression21 false;;
Printf.printf "\n" ;;





(**** Partie pour la machine SECD Concurrente V3 ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression22 = let open LangISWIMCv3.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression23 = let open LangISWIMCv3.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression18 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression24 = let open LangISWIMCv3.ISWIM in (App(Signal_ISWIM "signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression23;App(Signal_ISWIM "coucou"
,Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal")))))) ;;

(*  Correspond à l'expression suivante :  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression18 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) *)
let expression25 = let open LangISWIMCv3.ISWIM in (Catch_ISWIM(12,App(Signal_ISWIM "signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression23;App (Signal_ISWIM "coucou"
,Present_ISWIM("usdhfozeih",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))),("uygig",Const 7685))) ;;





Printf.printf "\n\n\n\n\nTest de la Machine SECD concurrente version 3 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv3 expression22 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv3 expression23 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  Signal( signal , ( Spawn( Present( signal , ( + expression13 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) \n" ;;
lancerSECDCv3 expression24 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression13 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) \n" ;;
lancerSECDCv3 expression25 false;;
Printf.printf "\n" ;;




(**** Partie pour la machine TTS ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression26 = let open LangISWIMTTS.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression27 = let open LangISWIMTTS.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(* Correspond à l'expression suivante : ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) *)
let expression28 = let open LangISWIMTTS.ISWIM in (App(App(Abs("s",Abs("s1",App(App(Spawn_ISWIM(Present_ISWIM("s",Const 6,Const 9)) , Spawn_ISWIM(Present_ISWIM("s1",Const 3,Const 5))), 
                                                  Spawn_ISWIM(Emit_ISWIM("s"))))), Signal_ISWIM), Signal_ISWIM));;





printf "\n\n\n\n\nTest de la MachineTTS\n\n" ;;

printf "On teste pour : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTS expression26 false ;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTS expression27 false;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) \n" ;;
startTTS expression28 false;;
printf "\n" ;;






(**** Partie pour la machine TTSI ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression29 = let open LangISWIMTTSI.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression30 = let open LangISWIMTTSI.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante : ( (lam x.( ( Spawn( put( x 4) ) Spawn( put( x 3 ) ) ) Spawn( wait get( x 1 0 ) ) )) init ) *)
let expression31 = let open LangISWIMTTSI.ISWIM in (App(Abs("x",(App(App(Spawn_ISWIM(Put_ISWIM("x", 4)),Spawn_ISWIM(Put_ISWIM("x",3))),Spawn_ISWIM(App(Wait,Get_ISWIM("x",1,0)))))),Signal_ISWIM));;





printf "\n\n\n\n\nTest de la MachineTTSI\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSI expression29 false ;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSI expression30 false;;
printf "\n" ;;

printf "On teste l'expression  ( (lam x.( ( Spawn( put( x 4) ) Spawn( put( x 3 ) ) ) Spawn( wait get( x 1 0 ) ) )) init ) \n" ;;
startTTSI expression31 false;;
printf "\n" ;;