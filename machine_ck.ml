open String ;;
open Printf ;;
open List ;;
open Machine_cc.CCMachine ;;
open Iswim.ISWIM ;;

(* Module qui implémente la machine CK *)
module CKMachine =
  struct

    (**** Types ****)

    (* Type représentant la continuation *)
    type k = 
        Fun of expr * k 
      | Arg of expr * k
      | Opd of (expr list * operateur) * (expr list) * k
      | MT

    (* Type représentant la machine CK *)
    type machine = Machine of controle * k





    (**** Affichage ****)


    (* Convertit le registre en chaîne de caractère *)
    let rec string_of_registre registre =
      match registre with 
          (Fun(expr,mt))                         ->   "(fun , "^(string_of_expr expr)^" , "^(string_of_registre mt)^")"

        | (Arg(expr,mt))                         ->   "(arg , "^(string_of_expr expr)^" , "^(string_of_registre mt)^")"

        | (Opd((liste_expr,op),liste_expr1,mt))  ->   "(opd , ["^(concat_string_liste( map string_of_expr liste_expr ))^", "^(string_of_operateur op)^"] , [ "
                                                                ^(concat_string_liste( map string_of_expr liste_expr1 ))^"] , "^(string_of_registre mt)^")"
        | MT                                     ->   "mt"


    (* Convertit un état de la machine Ck en chaîne de caractère *)
    let string_of_machine machine = 
      match machine with
        Machine(expr,registre) -> "("^(string_of_expr expr)^" , "^(string_of_registre registre)^")\n" 


    (* Affiche un état de la machine CK *)
    let afficherCK machine = printf "MachineCK : %s" (string_of_machine machine)






    (**** Fonctions utiles ****)

    (* Retourne le 1er élément d'une liste *)
    let initCalc liste = 
      match liste with
        | []    ->   raise FormatOpErreur
        
        | h::t  ->   (h,t)





    (**** Machine CK ****)

    (* Applique une transition de la machine CK pour un état donné *)
    let transition machine = 
      match machine with
          Machine(App(expr1,expr2),mt)              ->   Machine(expr1,Arg(expr2,mt)) 

        | Machine(Op(op,liste_expr),mt)             ->   let (elem,new_liste) = initCalc liste_expr in Machine(elem,Opd(([],op),new_liste,mt))

        | Machine(var,Fun(Abs(abs,expr),mt))        ->   Machine((reduction abs expr var),mt)

        | Machine(var,Arg(expr,mt))                 ->   Machine(expr,Fun(var,mt))

        | Machine(var,Opd((variables,op),[],mt))    ->   let new_liste = var::variables in 
                                                              if (for_all estConst variables) 
                                                                  then Machine((calcul op (rev (convert_liste_expr_liste_int new_liste))),mt) 
                                                                  else raise FormatOpErreur

        | Machine(var,Opd((variables,op),h::t,mt))  ->   Machine(h,Opd((var::variables,op),t,mt))

        | _                                           ->   raise EtatInconnu

    (* Applique les règles de la machine CK en affichant les étapes *)
    let rec machine etat afficher= 
      match etat with
          Machine(Const b,MT)        ->   Const b
        
        | Machine(Abs(abs,expr),MT)  ->   Abs(abs,expr)

        | indetermine                ->   if (afficher) then (afficherCK indetermine) else printf ""; machine (transition indetermine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerCK expression afficher = printf "Le résultat est %s \n" (string_of_expr (machine (Machine(expression,MT)) afficher))

  end