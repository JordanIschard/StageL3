open Printf ;;
open Lambda.LambdaCalcul ;;
open Machine_cc.CCMachine ;;
open Machine_scc.SCCMachine ;;
open Machine_ck.CKMachine ;;
open Machine_cek.CEKMachine ;;
open Machine_secd.SECDMachine ;;
open Machine_secdCv1.SECDCv1Machine;;
open Machine_secdCv2.SECDCv2Machine;;
open Machine_secdCv3.SECDCv3Machine;;
open Machine_secdCv4.SECDCv4Machine;;
open Machine_ttsv1.MachineTTS;;
open Machine_ttsv2.MachineTTS;;
open Machine_ttsv3.MachineTTS;;
open Machine_ttsiv1.MachineTTSI ;;
open Machine_ttsiv2.MachineTTSI ;;
open Machine_ttsiv3.MachineTTSI ;;
open Machine_ttsiv4.MachineTTSI ;;
open Machine_ttsiv5.MachineTTSI ;;
open Machine_ttsihv1.MachineTTSIH ;;
open Machine_ttsihv2.MachineTTSIH ;;
open Machine_ttsihv3.MachineTTSIH ;;
open Machine_ttsihv4.MachineTTSIH ;;

(* Test *)

(* Correspond à l'expression suivante : ( (lam w.( w ( + 2 w ) ))  2 ) *)
let expression   = let open Iswim.ISWIM in App((Abs("w",(App(Var "w",Op(Add,[Const 2;Var "w"]))))),Const 2) ;;

(* Correspond à l'expression suivante : ( (lam x.( x y )) z ) *)
let expression1  = let open Iswim.ISWIM in App(Abs("x",App(Var "x", Var "y" )),Var "z");;

(*  Correspond à l'expression suivante : ( (lam x.lam y.( x y )) (lam y.( y f )) ) *)
let expression2  = let open Iswim.ISWIM in App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f")));;

(*  Correspond à l'expression suivante : ( (lam z.lam b.( z b )) (lam a.( a g )) ) *)
let expression3  = let open Iswim.ISWIM in App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g")));;

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression4  = let open Iswim.ISWIM in App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"])))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression5  = let open Iswim.ISWIM in App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1);;

(*  Correspond à l'expression suivante : ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) *)
let expression6  = let open Iswim.ISWIM in App(Abs("x",App(Var "x",Var "x")), Abs("y",App(Abs("x",Var "x"),Abs("x",Var "x"))));;

(*  Correspond à l'expression suivante : ( +++ ( (lam x.x) 2 ) ( (lam x.x) 3 ) 4 ( (lam x.x) 5 ) ) *)
let expression7  = let open Iswim.ISWIM in Op(Add4,[App(Abs("x",Var "x"),Const 2); App(Abs("x",Var "x"),Const 3); Const 4;App(Abs("x",Var "x"),Const 5)]);;

(* Correspond à l'expression suivante : ( (lam x.lam y.( x y )) (lam y.( y y )) ) *)
let expression8  = App_term(Abs_term("x",Abs_term("y",App_term(Var_term "x", Var_term "y"))),Abs_term("y",App_term(Var_term "y",Var_term "y")));;

(* Correspond à l'expression suivante : ( (lam w.( w ( + 2 w ) )) 2 ) *)
let expression9  = let open IswimE.ISWIM in (App((Abs("w",(App(Var "w",Op(Add,[Const 2;Var "w"]))))),Const 2)) ;;

(* Correspond à l'expression suivante : ( (lam x.( x  y )) z ) *)
let expression10 = let open IswimE.ISWIM in (App(Abs("x",App(Var "x", Var "y" )),Var "z"));;

(* Correspond à l'expression suivante : ( (lam x.(lam y.( x y ))) (lam y.( y f )) ) *)
let expression11 = let open IswimE.ISWIM in (App(Abs("x",Abs("y",App(Var "x", Var "y"))),Abs("y",App(Var "y",Var "f"))));;

(* Correspond à l'expression suivante : ( (lam z.(lam b.( z b ))) (lam a.( a g )) ) *)
let expression12 = let open IswimE.ISWIM in (App(Abs("z",Abs("b",App(Var "z", Var "b"))),Abs("a",App(Var "a",Var "g"))));;

(* Correspond à l'expression suivante : ( (lam w.( - w ))  ( ( (lam x.( x 10 )) (lam y.( + y )) ) ) *)
let expression13 = let open IswimE.ISWIM in (App(Abs("w",Op(Sub,[Var "w"])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Op(Add,[Var "y"])))));;

(* Correspond à l'expression suivante : ( ( (lam f.(lam x.( f x ))) (lam y.( + y y )) ) 1 ) *)
let expression14 = let open IswimE.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;






(**** Partie lambda-calcul ****)
(* Test de l'implantation des lambda-calculs en Ocaml *)

printf "\n\n\n\n\nTest des Lambda-calculs \n\n" ;;

afficherTerme expression8 ;;
let resT = beta_reduction_term expression8 ;;
afficherTerme resT ;;
printf "\n" ;;





(**** Partie ISWIM ****)
(* Test de l'implantation du langage ISWIM en Ocaml *)

printf "\n\n\n\n\nTest du langage ISWIM \n\n" ;; 

printf "Réduction pour ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
let open Iswim.ISWIM in n_red expression4 ;;
printf "\n" ;;


printf "Réduction pour ( (lam x.( x x )) (lam y.( (lam x.x) (lam x.x) )) ) \n" ;;
let open Iswim.ISWIM in n_red expression6;;
printf "\n" ;;





(**** Partie ISWIM avec erreur ****)
(* Implantation du langage ISWIM avec une gestion des erreurs *)

printf "\n\n\n\n\nTest du langage ISWIM avec erreur \n\n" ;; 

printf "Réduction pour  ( (lam w.( w ( + 2 w ) )) 2 ) \n" ;;
let open IswimE.ISWIM in n_red expression9 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam x.( x  y )) z ) \n" ;;
let open IswimE.ISWIM in n_red expression10 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam x.(lam y.( x y ))) (lam y.( y f )) ) \n" ;;
let open IswimE.ISWIM in n_red expression11 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam z.(lam b.( z b ))) (lam a.( a g )) ) \n" ;;
let open IswimE.ISWIM in n_red expression12 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( (lam w.( - w ))  ( ( (lam x.( x 10 )) (lam y.( + y )) ) ) \n" ;;
let open IswimE.ISWIM in n_red expression13 ;;
Printf.printf "\n\n" ;;

printf "Réduction pour ( ( (lam f.(lam x.( f x ))) (lam y.( + y y )) ) 1 ) \n" ;;
let open IswimE.ISWIM in n_red expression14 ;;
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
let expression14 = let open Lang_secdCv1.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression15 = let open Lang_secdCv1.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression4 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression16 = let open Lang_secdCv1.ISWIM in 
(Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression14;Signal_ISWIM("coucou",Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))));;





printf "\n\n\n\n\nTest de la MachineSECD concurrente version 1 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv1 expression14 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv1 expression15 false;;
printf "\n" ;;

printf "On teste l'expression  Signal( signal , ( Spawn( Present( signal , ( + expression15 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) \n" ;;
lancerSECDCv1 expression16 false;;
printf "\n" ;;





(**** Partie pour la machine SECD Concurrente V2 ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression17 = let open Lang_secdCv2.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression18 = let open Lang_secdCv2.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression4 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression19 = let open Lang_secdCv2.ISWIM in (Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression17;Signal_ISWIM("coucou",
Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))));;

(*  Correspond à l'expression suivante :  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression4 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) *)
let expression20 = let open Lang_secdCv2.ISWIM in (Catch_ISWIM(6,Signal_ISWIM("signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression17;
Signal_ISWIM("coucou",Present_ISWIM("usdhfozeih",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))),("uygig",Const 7685))) ;;





Printf.printf "\n\n\n\n\nTest de la Machine SECD concurrente version 2 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv2 expression17 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv2 expression18 false;;
printf "\n" ;;

printf "On teste l'expression  Signal( signal , ( Spawn( Present( signal , ( + expression17 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) \n" ;;
lancerSECDCv2 expression19 false;;
printf "\n" ;;

printf "On teste l'expression  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression17 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) \n" ;;
lancerSECDCv2 expression20 false;;
printf "\n" ;;





(**** Partie pour la machine SECD Concurrente V3 ****)
(* Elle *)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression21 = let open Lang_secdCv3.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression22 = let open Lang_secdCv3.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante : signal s in ( Spawn( present s in ( + expression22 ( signal s1 in ( present s1 in 5 65 ) ) ) 3 ) Spawn( emit s ) ) *)
let expression23 = let open Lang_secdCv3.ISWIM in (Signal_ISWIM("s",(App((Spawn(Present_ISWIM("s",Op(Add,[expression22;Signal_ISWIM("s1",Present_ISWIM("s1",Const 5,Const 65))]),Const 3)))
,(Spawn(Emit_ISWIM "s"))))));;

(*  Correspond à l'expression suivante : try ( signal s in ( Spawn( present s in ( + expression22 ( signal s1 in ( present s2 in 5 65 ) ) ) 3 ) Spawn( emit s ) ) ) catch 8 ( 7685 ) *)
let expression24 = let open Lang_secdCv3.ISWIM in (Catch_ISWIM(8,Signal_ISWIM("s",(App((Spawn(Present_ISWIM("s",Op(Add,[expression22;Signal_ISWIM("s1",Present_ISWIM("s2",Const 5,Const 65))]),Const 3)))
,(Spawn(Emit_ISWIM "s"))))),("uygig",Const 7685))) ;;

(*  Correspond à l'expression suivante : signal s in ( Spawn( signal s1 in ( present s1 in ( 98 ( ( - get( s 2 ) get( s 2 ) ) get( s 2 ) ) ) ) ) ( Spawn( put( s 6 ) ( put( s 3 ) emit s ) ) 
Spawn( signal s1 in ( present s1 in ( 98 ( ( - get( s 2 ) get( s 2 ) ) get( s 2 ) ) ) ) ) ) ) *)
let expression25 = let open Lang_secdCv3.ISWIM in Signal_ISWIM("s",App(Spawn(Signal_ISWIM("s1",Present_ISWIM("s1",Const 98,App(Op(Sub,[Get_ISWIM("s",2);Get_ISWIM("s",2)]),Get_ISWIM("s",2)))))
,App(Spawn(App(Put_ISWIM("s",6),App(Put_ISWIM("s",3),Emit_ISWIM "s"))),Spawn(Signal_ISWIM("s1",Present_ISWIM("s1",Const 98,App(Op(Add,[Get_ISWIM("s",2);Get_ISWIM("s",2)]),Get_ISWIM("s",2))))))));;





printf "\n\n\n\n\nTest de la Machine SECD concurrente version 3 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv3 expression21 false;;
printf "\n" ;;
                              
printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv3 expression22 false;;
printf "\n" ;;
                              
printf "On teste l'expression  signal s in ( Spawn( present s in ( + expression23 ( signal s1 in ( present s1 in 5 65 ) ) ) 3 ) Spawn( emit s ) ) \n" ;;
lancerSECDCv3 expression23 false;;
printf "\n" ;;
                              
printf "On teste l'expression  try ( signal s in ( Spawn( present s in ( + expression23 ( signal s1 in ( present s2 in 5 65 ) ) ) 3 ) Spawn( emit s ) ) ) catch 8 ( 7685 ) \n" ;;
lancerSECDCv3 expression24 false;;
printf "\n" ;;
                              
printf "On teste l'expression  signal s in ( Spawn( signal s1 in ( present s1 in ( 98 ( ( - get( s 2 ) get( s 2 ) ) get( s 2 ) ) ) ) ) ( Spawn( put( s 6 ) ( put( s 3 ) emit s ) ) 
Spawn( signal s1 in ( present s1 in ( 98 ( ( - get( s 2 ) get( s 2 ) ) get( s 2 ) ) ) ) ) ) ) \n" ;;
lancerSECDCv3 expression25 false;;
printf "\n" ;;                          




(**** Partie pour la machine SECD Concurrente V4 ****)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression27 = let open Lang_secdCv4.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression28 = let open Lang_secdCv4.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante :  Signal( signal , ( Spawn( Present( signal , ( + expression18 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) *)
let expression29 = let open Lang_secdCv4.ISWIM in (App(Signal_ISWIM "signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression28;App(Signal_ISWIM "coucou"
,Present_ISWIM("coucou",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal")))))) ;;

(*  Correspond à l'expression suivante :  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression18 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) *)
let expression30 = let open Lang_secdCv4.ISWIM in (Catch_ISWIM(12,App(Signal_ISWIM "signal",(App((Spawn_ISWIM(Present_ISWIM("signal",Op(Add,[expression28;App (Signal_ISWIM "coucou"
,Present_ISWIM("usdhfozeih",Const 5,Const 65))]),Const 3))),(Spawn_ISWIM(Emit_ISWIM "signal"))))),("uygig",Const 7685))) ;;





Printf.printf "\n\n\n\n\nTest de la Machine SECD concurrente version 4 \n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
lancerSECDCv4 expression27 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
lancerSECDCv4 expression28 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  Signal( signal , ( Spawn( Present( signal , ( + expression13 Signal( coucou , Present( coucou , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) \n" ;;
lancerSECDCv4 expression29 false;;
Printf.printf "\n" ;;

printf "On teste l'expression  Catch( 6 , Signal( signal , ( Spawn( Present( signal , ( + expression13 Signal( coucou , Present( usdhfozeih , 5 , 65 ) ) ) , 3 ) ) Spawn( emit signal ) ) ) 
, (lam uygig.7685) ) \n" ;;
lancerSECDCv4 expression30 false;;
Printf.printf "\n" ;;





(**** Partie pour la machine TTS version 1 ****)
(* Variante de la machine SECD avec concurrence *)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression31 = let open Lang_tts.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante : ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression32 = let open Lang_tts.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante : ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) *)
let expression33 = let open Lang_tts.ISWIM in (App(App(Abs("s",Abs("s1",App(App(Spawn(Present("s",Const 6,Const 9)) , Spawn(Present("s1",Const 3,Const 5))),Spawn(Emit("s"))))), Signal), Signal));;





printf "\n\n\n\n\nTest de la MachineTTS version 1\n\n" ;;

printf "On teste l'expression  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSv1 expression31 false ;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSv1 expression32 false;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) \n" ;;
startTTSv1 expression33 false;;
printf "\n" ;;






(**** Partie pour la machine TTS version 2 ****)
(* Version qui modifie règle de la création de thread en ne donnant plus S et D  *)


printf "\n\n\n\n\nTest de la MachineTTS version 2\n\n" ;;

printf "On teste l'expresion  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSv2 expression31 false ;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSv2 expression32 false;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) \n" ;;
startTTSv2 expression33 false;;
printf "\n" ;;





(**** Partie pour la machine TTS version 3 ****)


printf "\n\n\n\n\nTest de la MachineTTS version 3\n\n" ;;

printf "On teste l'expresion  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSv3 expression31 false ;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSv3 expression32 false;;
printf "\n" ;;
                  
printf "On teste l'expression  ( ( (lam s.lam s1.( ( Spawn( Present s in 6 9 ) Spawn( Present s1 in 3 5 ) ) Spawn( emit s ) )) init ) init ) \n" ;;
startTTSv3 expression33 false;;
printf "\n" ;;





(**** Partie pour la machine TTSI version 1 ****)

(* Première version de la machine TTSI qui est une amélioration de la machine TTS avec l'ajout de valeurs partagées *)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression34 = let open Lang_ttsi.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression35 = let open Lang_ttsi.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante : ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) *)
let expression36 = let open Lang_ttsi.ISWIM in (App(Abs("s",App(App(Abs("id",Spawn(App(Wait,App(Abs("x",Var "x"),Get("s","id",0))))),Spawn(Put("s",3))),Put("s",4))),Signal));;

(*  Correspond à l'expression suivante : ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) *)
let expression37 = let open Lang_ttsi.ISWIM in (App(Abs("s",App(App(Spawn(App(Wait,App(Abs("x",Var"x"),Get("s","main",0)))),Spawn(Put("s",3))),Put("s",4))),Signal));;

(*  Correspond à l'expression suivante : ( Rec(f, lam n.( if ( = n 0 ) 0 ( + n ( f ( - n 1 ) ) ) ) ) 4 ) *)
let expression38 = let open Lang_ttsi.ISWIM in App(Rec("f",Abs("n",If(Op(Egal,[Var "n" ; Const 0]),Const 0,Op(Add,[Var "n" ; App(Var "f",Op(Sub,[Var "n" ; Const 1]))])))),Const 4);;

(*  Correspond à l'expression suivante : ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(0 3 ( Build(1 1 (1)) 3 Build(1 1 (4)) )) ) *)
let expression39 = let open Lang_ttsi.ISWIM in App( Abs("arbre",Match("arbre",[(Pattern(0,[Variable "a";Variable "i";Variable "a1"]),Abs("x",Abs("y",App(Var "y",Const 1))));(Pattern(1,[Variable "i"])
                                                ,Abs("x",Abs("y",App(Var "x",Const 1))))])), Build(0,3,[Build(1,1,[Const 1]);Const 3;Build(1,1,[Const 4])]));;

(*  Correspond à l'expression suivante : ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(1 1 (1)) ) *)
let expression40 = let open Lang_ttsi.ISWIM in App( Abs("arbre",Match("arbre",[(Pattern(0,[Variable "a";Variable "i";Variable "a1"]),Abs("x",Abs("y",App(Var "y",Const 1))));(Pattern(1,[Variable "i"])
                                                ,Abs("x",Abs("y",App(Var "x",Const 1))))])), Build(1,1,[Const 1]));;

(*  Correspond à l'expression suivante : ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),(+ a i)) ; (Pattern(1 [i]),i) ] )  Build(0 3 ( Build(1 1 (1)) 3 Build(1 1 (4)) )) ) *)
let expression41 = let open Lang_ttsi.ISWIM in App( Abs("arbre",Match("arbre",[(Pattern(0,[Pattern(1,[Variable "i2"]);Variable "i";Variable "a1"]),Op(Add,[Var "i2";Var "i"]));(Pattern(1,[Variable "i"])
                                                ,Var "i")])), Build(0,3,[Build(1,1,[Const 1]);Const 3;Build(1,1,[Const 4])]));;

(*  Correspond à l'expression suivante : ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),i) ] )  Build(1 1 (1)) ) *)
let expression42 = let open Lang_ttsi.ISWIM in App( Abs("arbre",Match("arbre",[(Pattern(0,[Variable "a";Variable "i";Variable "a1"]),Abs("x",Abs("y",App(Var "y",Const 1))));(Pattern(1,[Variable "i"])
                                                ,Var "i")])), Build(1,1,[Const 1]));;



printf "\n\n\n\n\nTest de la MachineTTSI version 1\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIv1 expression34 false ;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIv1 expression35 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv1 expression36 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv1 expression37 false;;
printf "\n" ;;





(**** Partie pour la machine TTSI version 2 ****)
(* Version qui modifie la règle de création de thread en retournant l'identifiant du thread créer ainsi que l'émission qui a été rajouté *)


printf "\n\n\n\n\nTest de la MachineTTSI version 2\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIv2 expression34 false ;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIv2 expression35 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv2 expression36 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv2 expression37 false;;
printf "\n" ;;





(**** Partie pour la machine TTSI version 3 ****)
(* Version qui modifie la règle de la création de thread en retournant l'identifiant du thread créer et en ne donnant plus S et D ainsi que l'émission qui a été rajouté *)


printf "\n\n\n\n\nTest de la MachineTTSI version 3\n\n" ;;

printf "On teste l'expression ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIv3 expression34 false ;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIv3 expression35 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv3 expression36 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv3 expression37 false;;
printf "\n" ;;





(**** Machine TTSI version 4 ****)

printf "\n\n\n\n\nTest de la MachineTTSI version 4\n\n" ;;


printf "On teste l'expression  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIv4 expression34 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIv4 expression35 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( (+ (get s id 0) 10) ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv4 expression36 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( lam s.( Spawn( lam x.x (get s main 0) ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv4 expression37 false;;
printf "\n" ;;


printf "On teste l'expression  ( Rec(f, lam n.( if ( = n 0 ) 0 ( + n ( f ( - n 1 ) ) ) ) ) 4 ) \n" ;;
startTTSIv4 expression38 false;;
printf "\n" ;;

printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(0 3 ( Build(1 1 (1)) 3 Build(1 1 (4)) )) ) \n" ;;
startTTSIv4 expression39 false;;
printf "\n" ;;

printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(1 1 (1)) ) \n" ;;
startTTSIv4 expression40 false;;
printf "\n" ;;

printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),(+ a i)) ; (Pattern(1 [i]),i) ] )  Build(0 3 ( Build(1 1 (1)) 3 Build(1 1 (4)) )) ) \n" ;;
startTTSIv4 expression41 false;;
printf "\n" ;;





(**** Machine TTSI version 5 ****)

printf "\n\n\n\n\nTest de la MachineTTSI version 5\n\n" ;;

printf "On teste l'expression  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIv5 expression34 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIv5 expression35 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( (+ (get s id 0) 10) ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv5 expression36 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( lam s.( Spawn( lam x.x (get s main 0) ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIv5 expression37 false;;
printf "\n" ;;


printf "On teste l'expression  ( Rec(f, lam n.( if ( = n 0 ) 0 ( + n ( f ( - n 1 ) ) ) ) ) 4 ) \n" ;;
startTTSIv5 expression38 false;;
printf "\n" ;;


printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(0 3 ( Build(1 1 (1)) 3 Build(1 1 (4)) )) ) \n" ;;
startTTSIv5 expression39 false;;
printf "\n" ;;


printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(1 1 (1)) ) \n" ;;
startTTSIv5 expression40 false;;
printf "\n" ;;


printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),i) ] )  Build(1 1 (1)) ) \n" ;;
startTTSIv5 expression42 false;;
printf "\n" ;;





(**** Partie pour la machine TTSIH ****)
(* Variante de la machine TTSI version 1 avec l'ajout de la gestion d'erreur *)

(*  Correspond à l'expression suivante : ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) *)
let expression43 = let open Lang_ttsih.ISWIM in (App(Abs("w",Op(Sub,[App(Var "w",Const 1);Const 5])),App(Abs("x",App(Var "x",Const 10)),Abs("y",Abs("z",Op(Add,[Var "z";Var "y"]))))));;

(*  Correspond à l'expression suivante :  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) *)
let expression44 = let open Lang_ttsih.ISWIM in (App(App(Abs("f",Abs("x",App(Var "f",Var "x"))),Abs("y",Op(Add,[Var "y";Var "y"]))),Const 1));;

(*  Correspond à l'expression suivante : ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) *)
let expression45 = let open Lang_ttsih.ISWIM in (App(Abs("s",App(App(Abs("id",Spawn(App(Wait,Get("s","id",0)))),Spawn(Put("s",3))),Put("s",4))),Signal));;

(*  Correspond à l'expression suivante : ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) *)
let expression46 = let open Lang_ttsih.ISWIM in (App(Abs("s",App(App(Spawn(App(Wait,Get("s","main",0))),Spawn(Put("s",3))),Put("s",4))),Signal));;

(*  Correspond à l'expression suivante : ( Rec(f, lam n.( if ( = n 0 ) 0 ( + n ( f ( - n 1 ) ) ) ) ) 4 ) *)
let expression47 = let open Lang_ttsih.ISWIM in App(Rec("f",Abs("n",If(Op(Egal,[Var "n" ; Const 0]),Const 0,Op(Add,[Var "n" ; App(Var "f",Op(Sub,[Var "n" ; Const 1]))])))),Const 4);; 





printf "\n\n\n\n\nTest de la MachineTTSIH\n\n" ;;

printf "On teste l'expression  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIH expression43 false ;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIH expression44 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIH expression45 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIH expression46 false;;
printf "\n" ;;






(**** Partie pour la machine TTSIH version 2 ****)
(* Variante de la machine TTSI version 2 avec l'ajout de la gestion d'erreur *)


printf "\n\n\n\n\nTest de la MachineTTSIH version 2 \n\n" ;;

printf "On teste l'expression  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIHv2 expression43 false ;;
printf "\n" ;;

printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIHv2 expression44 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( get s id 0 ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIHv2 expression45 false;;
printf "\n" ;;

printf "On teste l'expression  ( ( lam s.( Spawn( get s main 0 ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIHv2 expression46 false;;
printf "\n" ;;

printf "On teste l'expression  ( Rec(f, lam n.( if ( = n 0 ) 0 ( + n ( f ( - n 1 ) ) ) ) ) 4 ) \n" ;;
startTTSIHv2 expression47 false;;
printf "\n" ;;


(**** Partie pour la machine TTSIH version 3 ****)
(*  *)


printf "\n\n\n\n\nTest de la MachineTTSIH version 3 \n\n" ;;


printf "On teste l'expression  ( (lam w.( - ( w 1 ) 5 )) ( (lam x.( x 10 )) (lam y.lam z.( + z y )) ) ) \n" ;;
startTTSIHv3 expression34 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( (lam f.lam x.( f x )) (lam y.( + y y )) ) 1 ) \n" ;;
startTTSIHv3 expression35 false;;
printf "\n" ;;


printf "On teste l'expression  ( ( lam s.( lam id.( Spawn( (+ (get s id 0) 10) ) Spawn( put( s 3 ) ) ) put( s 4 ) ) init ) \n" ;;
startTTSIHv3 expression36 true;;
printf "\n" ;;


printf "On teste l'expression  ( ( lam s.( Spawn( lam x.x (get s main 0) ) Spawn( put( s 3 ) ) put( s 4 ) ) init ) \n" ;;
startTTSIHv3 expression37 false;;
printf "\n" ;;


printf "On teste l'expression  ( Rec(f, lam n.( if ( = n 0 ) 0 ( + n ( f ( - n 1 ) ) ) ) ) 4 ) \n" ;;
startTTSIHv3 expression38 false;;
printf "\n" ;;


printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(0 3 ( Build(1 1 (1)) 3 Build(1 1 (4)) )) ) \n" ;;
startTTSIHv3 expression39 false;;
printf "\n" ;;


printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),true) ] )  Build(1 1 (1)) ) \n" ;;
startTTSIHv3 expression40 false;;
printf "\n" ;;


printf "On teste l'expression  ( lam arbre.( match arbre with [ (Pattern(0 [a;i;a1]),false) ; (Pattern(1 [i]),i) ] )  Build(1 1 (1)) ) \n" ;;
startTTSIHv3 expression42 false;;
printf "\n" ;;
