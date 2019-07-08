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
      | Egal

    type exprISWIM = 
        Var of string 
      | Abs of string * exprISWIM 
      | Error of int 
      | App of exprISWIM * exprISWIM
      | Op of operateur * exprISWIM list
      | Const of int
      | Spawn of exprISWIM
      | Present of string * exprISWIM * exprISWIM
      | Signal
      | Put of string * int
      | Get of string * string * int
      | Wait
      | Catch of exprISWIM * (string * exprISWIM)
      | Throw
      | If of exprISWIM * exprISWIM * exprISWIM
      | Rec of string * exprISWIM



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

        | Egal       ->   "="

  end