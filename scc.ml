open String ;;
open Printf ;;
open List ;;
open Cc.CCMachine ;;
open LangISWIM.ISWIM ;;

(* Module qui implémente la machine SCC *)
module SCCMachine =
  struct

    (**** Affichage ****)

    (* Affiche une étape de la machine SCC *)
    let afficherSCC cs c =
     printf "MachineSCC : %s" (string_of_cc cs c)




    (**** Fonctions utiles ****)

    (* Retourne le 1er élément d'une liste *)
    let getPremElem liste = 
      match liste with
        | [] -> raise FormatOpErreur
        | h::t -> h

    (* Remplace le 1er élément par un Trou *)
    let trouPremElem liste =
      let rec aux premier liste =
        match liste with
          | [] -> []
          | h::t -> 
            if (premier) 
              then (Var "[ ]")::(aux false t)
              else h::(aux premier t)
      in aux true liste

    (* Le dernière élément d'une liste est un Trou *)
    let rec aTrouFinListe liste =
      match liste with
        [] -> false
        | [(Var "[ ]")] -> true
        | h::t -> aTrouFinListe t

    (* Donne le suivant d'un élément d'une liste *)
    let rec suivantDe expr liste =
      match liste with
        [] -> raise EtatInconnu
        | [h] -> raise EtatInconnu
        | h1::h2::t -> 
            if (equalExpr expr h1) 
              then h2
              else (suivantDe expr (h2::t))

    (* Remplace le suivant d'un élément par un Trou *)
    let rec trouSuivantDeElem expr liste =
      match liste with
        [] -> []
        | [h] -> [h]
        | h1::h2::t -> 
          if (equalExpr expr h1) 
            then h1::(Var "[ ]")::(trouSuivantDeElem expr t)
            else h1::(trouSuivantDeElem expr (h2::t))



            
    (**** Machine SCC ****)  

    (* Applique les règles de la machine SCC en affichant les étapes *)
    let rec machineSCC control_string context =
      let testContext context expr1 =
        match context with
          
          ((App(Abs(abs,expr2),Var "[ ]"))::t) -> machineSCC (reduction abs expr2 expr1) t

          | ((App(Var "[ ]", expr2))::t) -> machineSCC expr2 ((App(expr1,Var "[ ]"))::t)

          | (Op(op,liste_expr)::t) -> if (aTrouFinListe liste_expr) 
                                        then 
                                          begin
                                            let newListe = rempTrou expr1 liste_expr in
                                            if ( for_all estConst newListe )
                                              then machineSCC (calcul op (convert_liste_expr_liste_int newListe)) t
                                              else raise EtatInconnu 
                                          end
                                        else 
                                          begin
                                            let newListe = rempTrou expr1 liste_expr in
                                            machineSCC (suivantDe expr1 newListe) (Op(op,(trouSuivantDeElem expr1 newListe))::t)
                                          end

          | _ -> raise EtatInconnu
      in
      afficherSCC control_string context ;

      match (control_string,context) with 

      | (App(expr1,expr2),context) -> machineSCC expr1 ( append [(App(Var "[ ]" , expr2))] context)


      | (Op(op,liste_expr),context) -> machineSCC (getPremElem liste_expr) ( append [(Op(op,(trouPremElem liste_expr)))] context)

      | (Const b,context) -> 
          if ( length context) = 0 
            then Const b 
            else testContext context (Const b)
  
      | (Abs(abs,expr),context) -> 
          if ( length context) = 0 
            then Abs(abs,expr) 
            else testContext context (Abs(abs,expr))
      
      | ((Var var),context) -> testContext context (Var var)


    (* Lance et affiche le résultat de l'expression *)
    let lancerSCC expression =
       printf "Le résultat est %s \n" (string_of_expr (machineSCC expression []))

  end