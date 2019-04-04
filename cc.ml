open String;;
open LangISWIM.ISWIM;;



type expr =
  Term of exprISWIM
| Op of string * (expr list)
| Trou
;;

type  pile = expr list;;

type machineCC = Machine of expr * pile ;;

afficherList (libre (Abs("w",(App(Var "x",Op("+",[Var"y";Var "w"]))))));;
Printf.printf "coucou\n";;
Printf.printf "%s\n" (concat_string_liste ["jdfh";"ezkjfh"]);;
afficherExpr (Abs("w",(App(Var "x",Op("+",[Var"y";Var "w"])))));;

exception Nombre_De_Param_Erreur;;
exception Etat_Anormal;;
exception Pile_Vide;;
exception Operande_Inconnu;;

  
                                                    
