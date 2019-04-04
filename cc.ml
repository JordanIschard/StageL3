open String;;
open LangISWIM.ISWIM;;



type expr =
  Term of exprISWIM
| Op of string * (expr list)
| Trou
;;

type  pile = expr list;;

type machineCC = Machine of expr * pile ;;

let expression = (Abs("w",(App(Var "w",Op("+",[Var"y";Var "w"]))))) ;;
afficherList (libre expression);;
Printf.printf "coucou\n";;
Printf.printf "%s\n" (concat_string_liste ["jdfh";"ezkjfh"]);;
afficherExpr expression;;
Printf.printf "%b\n" (equal_expr expression expression);;

exception Nombre_De_Param_Erreur;;
exception Etat_Anormal;;
exception Pile_Vide;;
exception Operande_Inconnu;;

  
                                                    
