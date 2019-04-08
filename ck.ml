open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open LangISWIM.ISWIM ;;

(* La machine ck est une paire comportant une expression d'un côté et un etat une expression et un triplet de l'autre *)

module CKMachine =
  struct

    (**** Types ****)

    type k = 
      Fun of exprISWIM * k 
      | Arg of exprISWIM * k
      | Opd of (exprISWIM list * string) * (exprISWIM list) * k
      | MT

    (**** Exception ****)

    exception ContextInconnu 

    (**** Fonctions utiles ****)

    (* Vérifie si k est mt ,c'est-à-dire qu'il n'y ait plus rien dans le registre *)
    let estMT k = 
      match k with
      MT -> true
      | _ -> false

    (* Convertit le registre en chaîne de caractère *)
    let rec string_of_registre registre =
      match registre with 
        (Fun(expr,mt)) -> if (estMT mt) then "(fun , "^(string_of_expr expr)^" , mt)"
                                        else "(fun , "^(string_of_expr expr)^" , "^(string_of_registre mt)^")"
        | (Arg(expr,mt)) -> if (estMT mt) then "(arg , "^(string_of_expr expr)^" , mt)"
                                          else "(arg , "^(string_of_expr expr)^" , "^(string_of_registre mt)^")"
        | (Opd((liste_expr,op),liste_expr1,mt)) -> if (estMT mt) 
        then "(opd , ["^(concat_string_liste(List.map string_of_expr liste_expr ))^", "^op^"] , [ "^(concat_string_liste(List.map string_of_expr liste_expr1 ))^"] , mt)"
        else "(opd , ["^(concat_string_liste(List.map string_of_expr liste_expr ))^", "^op^"] , [ "^(concat_string_liste(List.map string_of_expr liste_expr1 ))^"] , "^(string_of_registre mt)^")"
        | MT -> "mt"


    (* Convertit un état de la machine Ck en chaîne de caractère *)
    let string_of_ck expr registre =
      "("^(string_of_expr expr)^" , "^(string_of_registre registre)^")\n" 

    (* Affiche un état de la machine CK *)
    let afficherCK expression registre =
        Printf.printf "MachineCK : %s" (string_of_ck expression registre)

    let estVide liste =
      match liste with 
        [] -> true
        | _ -> false

    let enleverTete liste =
      match liste with
      [] -> []
      | h::t -> t

    (**** Machine CK ****)

    let rec machineCK expression registre = 
      let testRegistre registre expr =
        match registre with

          (Fun((Abs(abs,expr1)),mt)) -> machineCK (reduction abs expr1 expr) mt

          | (Arg(expr1,mt)) -> machineCK expr1 (Fun(expr,mt))

          | (Opd((liste_expr,op),liste_expr1,mt)) -> 
            if (estVide liste_expr1)
              then 

                begin
                  let newliste = List.append [expr] liste_expr in
                  if (List.for_all estConst newliste)
                    then machineCK (calcul op (convert_liste_expr_liste_int newliste)) mt
                    else raise EtatInconnu
                end

              else 
                begin
                  let newliste = List.append [expr] liste_expr in
                  machineCK (getPremElem liste_expr1) (Opd((newliste,op),(enleverTete liste_expr1),mt))
                end

          | _ -> raise EtatInconnu

      in

      afficherCK expression registre ;

      match (expression,registre) with

      (App(expr1,expr2),mt) -> machineCK expr1 (Arg(expr2,mt)) 

      | (Op(op,liste_expr),mt) -> 
          begin
            match liste_expr with
              [] -> raise FormatOpErreur
              | [h] -> machineCK h (Opd(([],op),[],mt))
              | h::t -> machineCK h (Opd(([],op),t,mt))
          end

      | (Const b,registre) -> 
          if (estMT registre)
            then Const b 
            else testRegistre registre (Const b)
  
      | (Abs(abs,expr),registre) -> 
          if (estMT registre)
            then Abs(abs,expr) 
            else testRegistre registre (Abs(abs,expr))
      
      | ((Var var),registre) -> testRegistre registre (Var var)

    let lancerCK expression =
      Printf.printf "Le résultat est %s \n" (string_of_expr (machineCK expression MT))

  end