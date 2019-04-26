open Printf ;;
open List ;;
open Printf ;;
open LangISWIM.ISWIM;;

(* Module qui implémente la machine CC *)
module CCMachine =
  struct
    
    (**** Exception ****)

    exception EtatInconnu










    (**** Affichage ****)

    (* Convertit une expression et une liste d'expression en chaîne de caractère *)
    let string_of_cc cs c = "("^(string_of_expr cs )^" ,[ "^(concat_string_liste( map string_of_expr c ))^"])\n"

    (* Affiche une étape de la machine CC *)
    let afficherCC cs c = printf "MachineCC : %s" (string_of_cc cs c)

    








    (**** Fonctions utiles ****)

    (* Donne le 1er élément non variable d'une liste d'expression *)
    let rec premNonVarListe liste_expr =
      match liste_expr with
          []    ->   raise FormatOpErreur
        
        | h::t  ->   if (estVariable h) then premNonVarListe t else h


    (* Remplace le 1er élément non variable d'une liste d'expression par un Trou *)
    let trouPremNonVarListe liste_expr =
      let rec aux pas_pris liste =
        match liste with 
            []    ->   []
          
          | h::t  ->   if (estVariable h) then h::(aux pas_pris t) else if pas_pris 
                                                                            then (Var "[ ]")::(aux false t) else h::(aux pas_pris t)
    in 
    aux true liste_expr


    (* Remplace le 1er Trou de la liste d'expression par l'élément donné *)
    let rec rempTrou elem liste_expr =
      let rec aux pasChanger liste_expr =
        match liste_expr with
            []            ->   []
          
          | Var "[ ]"::t  ->   elem::(aux false t)
          
          | h::t          ->   h::(aux pasChanger t)
      in 
      aux true liste_expr 




    (**** Machine CC ****)

    (* Applique les règles de la machine CC en affichant les étapes *)
    let rec machineCC control_string context =
      let testContext context expr1 =
        match context with
          
            (App(expr2,Var "[ ]"))::t   ->   if (estVariable expr1) then machineCC (App(expr2,expr1)) t else machineCC expr1 ((App(expr2,Var "[ ]"))::t)

          | (App(Var "[ ]", expr2))::t  ->   if (estVariable expr1) then machineCC (App(expr1,expr2)) t else machineCC expr1 ((App(Var "[ ]",expr2))::t)

          | Op(op,liste_expr)::t        ->   if (estVariable expr1) then machineCC (Op(op,(rempTrou expr1 liste_expr))) t else machineCC expr1 (Op(op,liste_expr)::t)

          | _                           ->   raise EtatInconnu
      in
      afficherCC control_string context ;
      match (control_string,context) with 

          (App(Abs(abs,expr1),expr2),context)  ->   machineCC (reduction abs expr1 expr2) context

        | (App(expr1,expr2),context)           ->   if (estVariable expr1) 
                                                      then if (estVariable expr2) then raise EtatInconnu else machineCC expr2 (append [(App(expr2,Var "[ ]"))] context)
                                                      else machineCC expr1 (append [(App(Var "[ ]" , expr2))] context)


        | (Op(op,liste_expr),context)          ->   if (for_all estConst liste_expr) 
                                                      then try   machineCC (calcul op (convert_liste_expr_liste_int liste_expr)) context
                                                           with  FormatOpErreur -> raise EtatInconnu
                                                      else machineCC (premNonVarListe liste_expr) (append [(Op(op,(trouPremNonVarListe liste_expr)))] context)
        
        | (Const b,context)                    ->   if (length context) = 0 then Const b else testContext context (Const b)

        | (Abs(abs,expr),context)              ->   if (length context) = 0 then Abs(abs,expr) else testContext context (Abs(abs,expr))

        | (Var var ,context)                   ->   testContext context (Var var)


    (* Lance et affiche le résultat de l'expression *)
    let lancerCC expression = printf "Le résultat est %s \n" (string_of_expr (machineCC expression []))
      
  end

                                                    
