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
      | Error_ISWIM of int 
      | App of exprISWIM * exprISWIM
      | Op of operateur * exprISWIM list
      | Const of int
      | Spawn_ISWIM of exprISWIM
      | Present_ISWIM of string * exprISWIM * exprISWIM
      | Signal_ISWIM
      | Put_ISWIM of string * int
      | Get_ISWIM of string * int * int
      | Wait
      | Catch_ISWIM of exprISWIM * (string * exprISWIM)
      | Throw_ISWIM



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