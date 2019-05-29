open String ;;
open Printf ;;
open List ;;
open Cc.CCMachine ;;
open LangISWIM.ISWIM ;;

(* Module qui implémente la machine SCC *)
module SCCMachine =
  struct

    (**** Types ****)

    (* Type représentant la machine SCC *)
    type machineSCC = MachineSCC of controle * contexte





    (**** Affichage ****)

    (* Convertit une expression et une liste d'expression en chaîne de caractère *)
    let string_of_machineSCC machine = 
      match machine with
        MachineSCC(controle,contexte) -> "("^(string_of_expr controle )^" ,[ "^(concat_string_liste( map string_of_expr contexte ))^"])\n"


    (* Affiche une étape de la machine SCC *)
    let afficherSCC machine = printf "MachineSCC : %s" (string_of_machineSCC machine)





    (**** Fonctions utiles ****)

    (* Retourne le 1er élément d'une liste *)
    let getPremier liste = 
      match liste with
        | []    ->   raise FormatOpErreur
        
        | h::t  ->   (h,Var "[ ]"::t)


    (* Le dernière élément d'une liste est un Trou *)
    let rec aTrouFinListe liste =
      match liste with
          []             ->   false

        | [(Var "[ ]")]  ->   true
        
        | h::t           ->   aTrouFinListe t


    (* Donne le suivant d'un élément d'une liste *)
    let rec suivantDe expr liste =
      match liste with
          []               ->   raise EtatInconnu

        | [h]              ->   raise EtatInconnu

        | Var "[ ]"::h::t  ->   (h,expr::Var "[ ]"::t) 

        | h1::h2::t        ->   let (elem,new_liste) = suivantDe expr (h2::t) in (elem, append [h1] new_liste)





    (**** Machine SCC ****)  

    (* Applique une transition de la machine CC pour un état donné *)
    let transitionSCC machine =
      match machine with
          MachineSCC(App(expr1,expr2),contexte)                   ->   MachineSCC(expr1,App(Var "[ ]",expr2)::contexte)

        | MachineSCC(Op(op,liste_expr),contexte)                  ->   let (elem,new_liste) = getPremier liste_expr in MachineSCC(elem,(Op(op,new_liste))::contexte)

        | MachineSCC(var,App(Abs(abs,expr),Var "[ ]")::contexte)  ->   MachineSCC((reduction abs expr var),contexte)

        | MachineSCC(var,App(Var "[ ]",expr)::contexte)           ->   MachineSCC(expr,App(var,Var "[ ]")::contexte)

        | MachineSCC(var,Op(op,liste_expr)::contexte)             ->   if (aTrouFinListe liste_expr) 
                                                                          then let liste = rempTrou var liste_expr in

                                                                              if (for_all estConst liste) 
                                                                                  then MachineSCC(calcul op (convert_liste_expr_liste_int liste),contexte) 

                                                                                  else raise FormatOpErreur

                                                                          else let (elem,new_liste) = suivantDe var liste_expr in MachineSCC(elem,(Op(op,new_liste))::contexte)

        | _                                                       ->   raise EtatInconnu


    (* Applique les règles de la machine SCC en affichant les étapes *)
    let rec machineSCC machine afficher= 
      match machine with
          MachineSCC(Const b,[])        ->   Const b
        
        | MachineSCC(Abs(abs,expr),[])  ->   Abs(abs,expr)

        | machine                       ->   if (afficher) then (afficherSCC machine) else printf ""; machineSCC (transitionSCC machine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerSCC expression afficher = printf "Le résultat est %s \n" (string_of_expr (machineSCC (MachineSCC(expression,[])) afficher))

  end