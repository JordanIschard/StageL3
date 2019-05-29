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
      | Spawn of exprISWIM
      | Present of string * exprISWIM * exprISWIM
      | Emit of string 
      | Signal of string * exprISWIM










    (***** Exception *****)

    exception FormatOpErreur
    exception NotConstErreur










    (**** Affichage ****)

    (* Concatène tous les éléments d'une liste entre eux *)
    let rec concat_string_liste liste =
      match liste with
          []    ->   ""
        
        | h::t  ->   h^" "^(concat_string_liste t)


    (* Convertit un opérateur en chaîne de caractère *)
    let string_of_operateur op =
      match op with
          Add1    ->   "++"
        
        | Sub1    ->   "--"
        
        | IsZero  ->   "== 0"
        
        | Add     ->   "+"
        
        | Sub     ->   "-"
        
        | Mult    ->   "*"
        
        | Div     ->   "/"
      

    (* Convertit une expression en chaîne de caractère *)
    let rec string_of_expr expr =
      match expr with 
          Var var                       ->   var

        | Const const                   ->   string_of_int const

        | App(expr1,expr2)              ->   "("^(string_of_expr expr1)^" "^(string_of_expr expr2)^")"

        | Abs(abs,expr)                 ->   "(lam "^abs^"."^(string_of_expr expr)^")"

        | Op(op,liste_expr)             ->   "("^(string_of_operateur op)^" "^(concat_string_liste ( map string_of_expr  liste_expr))^")"

        | Spawn expr                    ->   "spawn ("^(string_of_expr expr)^")"

        | Present (signal,expr1,expr2)  ->   "present "^signal^" in "^(string_of_expr expr1)^" "^(string_of_expr expr2)

        | Emit signal                   ->   "emit "^signal 

        | Signal(signal,expr)           ->   "signal "^signal^" in "^(string_of_expr expr)


    (* Affiche une expression *)
    let afficherExpr expression = printf "%s\n" (string_of_expr expression) 
    









    (**** Fonctions utiles ****)

    (* Donne le nombre d'opérande requis pour utiliser l'opérateur *)
    let getNbrOperande op =
      match op with
          Add1 | Sub1 | IsZero    ->   1

        | Add | Sub | Mult | Div  ->   2


      (* On applique le calcul sur une liste d'entier et lève une exception si le nombre de paramètre est erroné  *)
      let calcul op liste_expr =
        match (op,liste_expr) with

            (Add1,[h])     ->   Const (h+1)

          | (Sub1,[h])     ->   Const (h-1)

          | (IsZero,[h])   ->   if h = 0 then Abs("x",Abs("y",Var "x")) else Abs("x",Abs("y",Var "y"))

          | (Add,[h;h1])   ->   Const (h+h1)

          | (Sub,[h;h1])   ->   Const (h-h1)
          
          | (Mult,[h;h1])  ->   Const (h*h1)
          
          | (Div,[h;h1])   ->   Const (h/h1) 
          
          | (_,_)          ->   raise FormatOpErreur

  end