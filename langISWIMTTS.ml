open Printf ;;
open String ;;
open List ;;

(* Module qui implémente le langage ISWIM *)
module ISWIM =
  struct

    (**** Types ****)
    
    type operateur = 
        Add1 
      | Sub1
      | IsZero
      | Add
      | Sub
      | Mult
      | Div 

    type exprISWIM = 
        Var of string 
      | Abs of string * exprISWIM 
      | App of exprISWIM * exprISWIM
      | Op of operateur * exprISWIM list
      | Const of int
      | Spawn_ISWIM of exprISWIM
      | Present_ISWIM of string * exprISWIM * exprISWIM
      | Emit_ISWIM of string
      | Signal_ISWIM










    (***** Exception *****)

    exception OpFormatError










    (**** Affichage ****)

    (* Concatène tous les éléments d'une liste entre eux *)
    let rec concat_string_liste liste =
      match liste with
          []    ->   ""

        | h::t  ->   h^" "^(concat_string_liste t)


    (* Convertit un opérateur en chaîne de caractère *)
    let string_of_operateur op =
      match op with
          Add1       ->   "++"

        | Sub1       ->   "--"

        | IsZero     ->   "== 0"

        | Add        ->   "+"

        | Sub        ->   "-"

        | Mult       ->   "*"

        | Div        ->   "/"

  end