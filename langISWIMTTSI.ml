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
      | HasNext

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
      | Put_ISWIM of string * int
      | Get_ISWIM of string * int * int
      | Wait










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

        | HasNext    ->   "hasNext"
    









    (**** Fonctions utiles ****)

    (* Donne le nombre d'opérande requis pour utiliser l'opérateur *)
    let getOperandNb op =
      match op with
          Add1 | Sub1 | IsZero | HasNext ->   1

        | Add | Sub | Mult | Div  ->   2


      (* On applique le calcul sur une liste d'entier et lève une exception si le nombre de paramètre est erroné  *)
      let calcul op liste_expr =
        match (op,liste_expr) with
            (Add1,[h])     ->   Const (h+1)

          | (HasNext,[h])  ->   Const 0

          | (Sub1,[h])     ->   Const (h-1)

          | (IsZero,[h])   ->   if h = 0 then Abs("x",Abs("y",Var "x")) else Abs("x",Abs("y",Var "y"))

          | (Add,[h;h1])   ->   Const (h+h1)

          | (Sub,[h;h1])   ->   Const (h-h1)
          
          | (Mult,[h;h1])  ->   Const (h*h1)
          
          | (Div,[h;h1])   ->   Const (h/h1) 
          
          | (_,_)          ->   raise OpFormatError

  end