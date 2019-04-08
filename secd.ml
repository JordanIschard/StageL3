open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open Ck.CKMachine ;;
open Cek.CEKMachine ;;
open LangISWIM.ISWIM ;;


module SECDMachine =
  struct


    type control_string = string

    type stack =
        Vide_S
      | Clause_secd of control_string * stack

    type env = Env of (string * control_string) list

    type dump =
        Vide_D
      | Save of stack * env * control_string * dump

     
    let rec concat_liste_secd liste =
      match liste with
        [] -> " "
        | h::t -> h^" "^(concat_liste_secd t)

    let rec secdLanguage_of_exprISWIM expression =
      match expression with
        (Const const) -> string_of_int const
        | (Var var) -> var
        | (App(expr1,expr2)) -> (secdLanguage_of_exprISWIM expr1)^" "^(secdLanguage_of_exprISWIM expr2)^" ap"
        | (Abs(abs,expr)) -> "("^abs^","^(secdLanguage_of_exprISWIM expr)^")"
        | (Op(op,liste_expr)) -> concat_liste_secd(List.map secdLanguage_of_exprISWIM liste_expr)^"prim "^op

  end