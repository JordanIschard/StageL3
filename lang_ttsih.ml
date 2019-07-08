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
      | Const of int
      | Error of int 

      | Abs of string * exprISWIM 
      | App of exprISWIM * exprISWIM
      | Op of operateur * exprISWIM list
      
      | Spawn of exprISWIM
      | Present of string * exprISWIM * exprISWIM
      | Signal
      | Wait

      | Put of string * int
      | Get of string * string * int
      
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