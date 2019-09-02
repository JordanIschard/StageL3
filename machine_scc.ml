open String ;;
open Printf ;;
open List ;;
open Machine_cc.CCMachine ;;
open Iswim.ISWIM ;;

(* Module qui implémente la machine SCC *)
module SCCMachine =
  struct

    (**** Types ****)

    (* Type représentant la machine SCC *)
    type machine = Machine of controle * contexte





    (**** Affichage ****)

    (* Convertit une expression et une liste d'expression en chaîne de caractère *)
    let string_of_machine machine = 
      match machine with
        Machine(controle,contexte) -> "("^(string_of_expr controle )^" ,[ "^(concat_string_liste( map string_of_expr contexte ))^"])\n"


    (* Affiche une étape de la machine SCC *)
    let afficherSCC machine = printf "MachineSCC : %s" (string_of_machine machine)





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

    (* Applique une transition de la machine SCC pour un état donné *)
    let transition machine =
      match machine with
          Machine(App(expr1,expr2),contexte)                   ->   Machine(expr1,App(Var "[ ]",expr2)::contexte)

        | Machine(Op(op,liste_expr),contexte)                  ->   let (elem,new_liste) = getPremier liste_expr in Machine(elem,(Op(op,new_liste))::contexte)

        | Machine(var,App(Abs(abs,expr),Var "[ ]")::contexte)  ->   Machine((reduction abs expr var),contexte)

        | Machine(var,App(Var "[ ]",expr)::contexte)           ->   Machine(expr,App(var,Var "[ ]")::contexte)

        | Machine(var,Op(op,liste_expr)::contexte)             ->   if (aTrouFinListe liste_expr) 
                                                                          then let liste = rempTrou var liste_expr in

                                                                              if (for_all estConst liste) 
                                                                                  then Machine(calcul op (convert_liste_expr_liste_int liste),contexte) 

                                                                                  else raise FormatOpErreur

                                                                          else let (elem,new_liste) = suivantDe var liste_expr in Machine(elem,(Op(op,new_liste))::contexte)

        | _                                                    ->   raise EtatInconnu


    (* Applique les règles de la machine SCC en affichant les étapes *)
    let rec machine etat afficher= 
      match etat with
          Machine(Const b,[])        ->   Const b
        
        | Machine(Abs(abs,expr),[])  ->   Abs(abs,expr)

        | indetermine                ->   if (afficher) then (afficherSCC indetermine) else printf ""; machine (transition indetermine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerSCC expression afficher = printf "Le résultat est %s \n" (string_of_expr (machine (Machine(expression,[])) afficher))

  end