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
        Fun of exprISWIM * k 
      | Arg of exprISWIM * k
      | Opd of (exprISWIM list * operateur) * (exprISWIM list) * k
      | MT

    (* Type représentant la machine CK *)
    type machineCK = MachineCK of controle * k





    (**** Affichage ****)

    (* Vérifie si k est mt ,c'est-à-dire qu'il n'y ait plus rien dans le registre *)
    let estMT k = 
      match k with
          MT  ->   true

        | _   ->   false


    (* Convertit le registre en chaîne de caractère *)
    let rec string_of_registre registre =
      match registre with 
          (Fun(expr,mt))                           ->   if (estMT mt) then "(fun , "^(string_of_expr expr)^" , mt)"
                                                                      else "(fun , "^(string_of_expr expr)^" , "^(string_of_registre mt)^")"

        | (Arg(expr,mt))                         ->   if (estMT mt) then "(arg , "^(string_of_expr expr)^" , mt)"
                                                                    else "(arg , "^(string_of_expr expr)^" , "^(string_of_registre mt)^")"

        | (Opd((liste_expr,op),liste_expr1,mt))  ->   if (estMT mt) 
                                                          then   "(opd , ["^(concat_string_liste( map string_of_expr liste_expr ))^", "
                                                                ^(string_of_operateur op)^"] , [ "^(concat_string_liste( map string_of_expr liste_expr1 ))^"] , mt)"
                                                          else   "(opd , ["^(concat_string_liste( map string_of_expr liste_expr ))^", "^(string_of_operateur op)^"] , [ "
                                                                ^(concat_string_liste( map string_of_expr liste_expr1 ))^"] , "^(string_of_registre mt)^")"
        | MT                                     ->   "mt"


    (* Convertit un état de la machine Ck en chaîne de caractère *)
    let string_of_machineCK machine = 
      match machine with
        MachineCK(expr,registre) -> "("^(string_of_expr expr)^" , "^(string_of_registre registre)^")\n" 


    (* Affiche un état de la machine CK *)
    let afficherCK machine = printf "MachineCK : %s" (string_of_machineCK machine)






    (**** Fonctions utiles ****)

    (* Retourne le 1er élément d'une liste *)
    let initCalc liste = 
      match liste with
        | []    ->   raise FormatOpErreur
        
        | h::t  ->   (h,t)





    (**** Machine CK ****)

    (* Applique une transition de la machine CK pour un état donné *)
    let transitionCK machine = 
      match machine with
          MachineCK(App(expr1,expr2),mt)              ->   MachineCK(expr1,Arg(expr2,mt)) 

        | MachineCK(Op(op,liste_expr),mt)             ->   let (elem,new_liste) = initCalc liste_expr in MachineCK(elem,Opd(([],op),new_liste,mt))

        | MachineCK(var,Fun(Abs(abs,expr),mt))        ->   MachineCK((reduction abs expr var),mt)

        | MachineCK(var,Arg(expr,mt))                 ->   MachineCK(expr,Fun(var,mt))

        | MachineCK(var,Opd((variables,op),[],mt))    ->   let new_liste = var::variables in 
                                                              if (for_all estConst variables) 
                                                                  then MachineCK((calcul op (rev (convert_liste_expr_liste_int new_liste))),mt) 
                                                                  else raise FormatOpErreur

        | MachineCK(var,Opd((variables,op),h::t,mt))  ->   MachineCK(h,Opd((var::variables,op),t,mt))

        | _                                           ->   raise EtatInconnu

    (* Applique les règles de la machine CK en affichant les étapes *)
    let rec machineCK machine afficher= 
      match machine with
          MachineCK(Const b,MT)        ->   Const b
        
        | MachineCK(Abs(abs,expr),MT)  ->   Abs(abs,expr)

        | machine                      ->   if (afficher) then (afficherCK machine) else printf ""; machineCK (transitionCK machine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerCK expression afficher = printf "Le résultat est %s \n" (string_of_expr (machineCK (MachineCK(expression,MT)) afficher))

  end