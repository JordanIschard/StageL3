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

    (* pattern utilisé pour faire du filtrage *)
    type pat = 
      Pattern_var of string                                (* on filtre avec une variable *)
    | Pattern_ISWIM of int * pat list                  (* on filtre avec un pattern *)

    type exprISWIM = 
        Var_ISWIM of string 
      | Abs of string * exprISWIM 
      | App of exprISWIM * exprISWIM
      | Op of operateur * exprISWIM list
      | Const of int
      | Spawn_ISWIM of exprISWIM
      | Present_ISWIM of string * exprISWIM * exprISWIM
      | Signal_ISWIM
      | Put_ISWIM of string * int
      | Get_ISWIM of string * string * int
      | Emit_ISWIM of string 
      | Wait
      | Rec of string * exprISWIM
      | If of exprISWIM * exprISWIM * exprISWIM
      | Build_ISWIM of int * exprISWIM list
      | Match of string * ( pat * exprISWIM ) list

    let vraie = Abs("x",Abs("y",App(Var_ISWIM "x",Const 1)))

    let faux = Abs("x",Abs("y",App(Var_ISWIM "y",Const 1)))

      exception DivByZero
      exception InvalidOpFormat

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


    let nbr_operande op =
      match op with
          Add1       ->   1

        | Sub1       ->   1

        | IsZero     ->   1

        | Add        ->   2

        | Sub        ->   2

        | Mult       ->   2

        | Div        ->   2

        | Egal       ->   2


    let calculer op operandes =
      match (op,operandes) with
          (Add1,[n])      ->   Const (n+1)

        | (Sub1,[n])      ->   Const (n-1)

        | (IsZero,[n])    ->   if n=0 then vraie else faux

        | (Add,[n;m])     ->   Const (n+m)

        | (Sub,[n;m])     ->   Const (n-m)

        | (Mult,[n;m])    ->   Const (n*m)

        | (Div,[n;m])     ->   if m = 0 then raise DivByZero else Const (n/m)

        | (Egal,[n;m])    ->   if n = m then vraie else faux

        | _               ->   raise InvalidOpFormat


  end