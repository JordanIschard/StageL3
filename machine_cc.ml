open Printf ;;
open List ;;
open Iswim.ISWIM;;

(* Module qui implémente la machine CC *)
module CCMachine =
  struct

    (**** Types ****)

    (* Type représentant la chaîne de contrôle de la machine CC *)
    type controle = expr


    (* Type représentant le contexte de la machine CC *)
    type contexte = expr list


    (* Type représentant la machine CC *)
    type machineCC = MachineCC of controle * contexte
    




    (**** Exception ****)

    exception EtatInconnu     (* L'état de la machine n'est pas normal *)





    (**** Affichage ****)

    (* Convertit une expression et une liste d'expression en chaîne de caractère *)
    let string_of_machineCC machine = 
      match machine with
        MachineCC(controle,contexte) -> "("^(string_of_expr controle )^" ,[ "^(concat_string_liste( map string_of_expr contexte ))^"])\n"


    (* Affiche une étape de la machine CC *)
    let afficherCC machine = printf "MachineCC : %s" (string_of_machineCC machine)

    



    (**** Fonctions utiles ****)

    (* Prend l'élément qui n'est pas une variable et le remplace par un Trou *)
    let rec nextElem liste_expr =
      match liste_expr with 
          []    ->   raise FormatOpErreur

        | h::t  ->   if (estVariable h) then let (elem,new_liste) = nextElem t in (elem,append [h] new_liste)
                                        else (h,append [Var "[ ]"] t)
    

    (* Remplace le Trou par un élément *)
    let rec rempTrou elem liste_expr =
      match liste_expr with
          []            ->   raise EtatInconnu

        | Var "[ ]"::t  ->   append [elem] t

        | h::t          ->   append [h] (rempTrou elem t)





    (**** Machine CC ****)

    (* Applique une transition de la machine CC pour un état donné *)
    let transitionCC machine =
      match machine with
          MachineCC(App(Abs(abs,expr1),expr2),contexte)   ->  if (estVariable expr2) then MachineCC((reduction abs expr1 expr2),contexte)  else MachineCC(expr2,(App(Abs(abs,expr1),Var "[ ]"))::contexte)
        
        | MachineCC(App(expr1,expr2),contexte)            ->  if (estVariable expr1) 
                                                                then if (estVariable expr2) then raise EtatInconnu else MachineCC(expr2,App(expr1,Var "[ ]")::contexte)
                                                                else MachineCC(expr1,App(Var "[ ]",expr2)::contexte)
        
        | MachineCC(Op(op,liste_expr),contexte)           ->  if (for_all estConst liste_expr)
                                                                 then if (length liste_expr = (getNbrOperande op)) 
                                                                          then MachineCC((calcul op (convert_liste_expr_liste_int liste_expr)),contexte)
                                                                          else raise FormatOpErreur
                                                                 else let (elem,new_liste) = nextElem liste_expr in MachineCC(elem,(Op(op,new_liste))::contexte)

        | MachineCC(expr,Op(op,liste_expr)::contexte)     ->  MachineCC(Op(op,(rempTrou expr liste_expr)),contexte)
        
        | MachineCC(expr2,App(expr1,Var "[ ]")::contexte) ->  MachineCC(App(expr1,expr2),contexte) 

        | MachineCC(expr1,App(Var "[ ]",expr2)::contexte) ->  MachineCC(App(expr1,expr2),contexte) 

        | _                                               ->  raise EtatInconnu


    (* Applique les règles de la machine CC en affichant les étapes *)
    let rec machineCC machine afficher= 
      match machine with
          MachineCC(Const b,[])        ->   Const b
        
        | MachineCC(Abs(abs,expr),[])  ->   Abs(abs,expr)

        | machine                      ->   if (afficher) then (afficherCC machine) else printf ""; machineCC (transitionCC machine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerCC expression afficher = printf "Le résultat est %s \n" (string_of_expr (machineCC (MachineCC(expression,[])) afficher))
      
  end

                                                    
