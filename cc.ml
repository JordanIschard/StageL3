open Printf ;;
open List ;;
open Printf ;;
open LangISWIM.ISWIM;;

(* Module qui implémente la machine CC *)
module CCMachine =
  struct

    type controle = exprISWIM
    type contexte = exprISWIM list

    type machineCC = MachineCC of controle * contexte
    
    (**** Exception ****)

    exception EtatInconnu










    (**** Affichage ****)

    (* Convertit une expression et une liste d'expression en chaîne de caractère *)
    let string_of_machineCC machine = 
      match machine with
        MachineCC(controle,contexte) -> "("^(string_of_expr controle )^" ,[ "^(concat_string_liste( map string_of_expr contexte ))^"])\n"

    (* Affiche une étape de la machine CC *)
    let afficherCC machine = printf "MachineCC : %s" (string_of_machineCC machine)

    








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
    let transition machine =
      match machine with
          MachineCC(App(Abs(abs,expr1),expr2),contexte)   ->  if (estVariable expr2) then MachineCC((reduction abs expr1 expr2),contexte)  else MachineCC(expr2,(App(Abs(abs,expr1),Var "[ ]"))::contexte)
        
        | MachineCC(App(expr1,expr2),contexte)            ->  if (estVariable expr1) 
                                                                then if (estVariable expr2) then raise EtatInconnu else MachineCC(expr2,App(expr1,Var "[ ]")::contexte)
                                                                else MachineCC(expr1,App(Var "[ ]",expr2)::contexte)
        
        | MachineCC(Op(op,liste_expr),contexte)           ->  if (for_all estConst liste_expr)
                                                                 then if (length liste_expr = (getNbrOperande op)) 
                                                                          then MachineCC((calcul op (convert_liste_expr_liste_int liste_expr)),contexte)
                                                                          else raise FormatOpErreur
                                                                 else MachineCC((premNonVarListe liste_expr),(Op(op,(trouPremNonVarListe liste_expr)))::contexte)

        | MachineCC(expr2,App(expr1,Var "[ ]")::contexte) ->  MachineCC(App(expr1,expr2),contexte) 

        | MachineCC(expr1,App(Var "[ ]",expr2)::contexte) ->  MachineCC(App(expr1,expr2),contexte) 

        | _ -> raise EtatInconnu



    (* Applique les règles de la machine CC en affichant les étapes *)
    let rec machineCC machine afficher= 
      match machine with
          MachineCC(Const b,[]) -> Const b
        
        | MachineCC(Abs(abs,expr),[]) -> Abs(abs,expr)

        | machine -> if (afficher) then (afficherCC machine) else printf ""; machineCC (transition machine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerCC expression afficher = printf "Le résultat est %s \n" (string_of_expr (machineCC (MachineCC(expression,[])) afficher))
      
  end

                                                    
