open Cc.CCMachine ;;
open LangISWIM.ISWIM ;;

(* Module qui implémente la machine SCC *)
module SCCMachine =
  struct

    (**** Fonctions utiles ****)

    (* Affiche une étape de la machine SCC *)
    let afficherSCC cs c =
      Printf.printf "MachineSCC : %s" (string_of_cc cs c)

    (**** Machine SCC ****)  

    (* Applique les règles de la machine SCC en affichant les étapes (En cours) *)
    let rec machineSCC control_string context =
      let testContext context expr1 =
        match context with
          
          ((App(Abs(abs,expr2),Var "[ ]"))::t) -> machineSCC (reduction abs expr1 expr2) t

          | ((App(Var "[ ]", expr2))::t) -> machineSCC expr2 ((App(expr1,Var "[ ]"))::t)

          | (Op(op,liste_expr)::t) -> if (estConst expr1) (* A continuer *)
                                        then machineSCC (Op(op,(rempTrou expr1 liste_expr))) t 
                                        else machineSCC expr1 (Op(op,liste_expr)::t)

          | _ -> raise EtatInconnu
      in
      afficherSCC control_string context ;

      match (control_string,context) with 

      | (App(expr1,expr2),context) -> machineSCC expr1 (List.append [(App(Var "[ ]" , expr2))] context)


      | (Op(op,liste_expr),context) ->  machineSCC (premNonVarListe liste_expr) (List.append [(Op(op,(trouPremNonVarListe liste_expr)))] context)
      
      | (control_string,context) -> 
        if (estVariable control_string) 
          then testContext context control_string 
          else raise EtatInconnu


    (* Lance et affiche le résultat de l'expression *)
    let lancerSCC expression =
      Printf.printf "Le résultat est %s \n" (string_of_expr (machineSCC expression []))
  end