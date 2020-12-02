open Printf ;;
open String ;;
open List ;;

(* Module qui implémente le langage ISWIM *)
module ISWIM =
  struct

    (**** Types ****)
    
    (* Opérateur du langage *)
    type operateur = 
        Add1 
      | Sub1
      | IsZero
      | Add
      | Sub
      | Mult
      | Div 
      | Egal


    (* Expression du langage *)
    type expr = 
 
        Var of string                           (* une variable x,y,z... *)
      | Const of int                            (* une constante n,m,... *)
      | Error of int                            (* une erreur e *)

      | Abs of string * expr                    (* Abstraction de la forme : lam string.(expr) *)
      | App of expr * expr                      (* Application de la forme : (expr1 expr2) *)
      | Op of operateur * expr list             (* Opération de la forme : (op expr_list) *)
      
      | Spawn of expr                           (* Création de thread : (expr) avec expr la partie de la chaîne prise par le nouveau thread *)
      | Present of string * expr * expr         (* Conditionnel pour un signal de la forme :  (string,expr,expr1) avec string le signal, expr la partie prise si le signal est émis et expr1 la partie prise si le signal n'est pas émis *)
      | Signal                                  (* Initialise *)
      | Wait                                    (* Met en attente *)

      | Put of string * int                     (* Ajoute une valeur de la forme : (string,int) avec string le signal et int la valeur à ajouter *)
      | Get of string * string * int            (* Prend une valeur de la forme : (string,string1,int) avec string le signal, string1 le thread et int l'élément neutre *)
      
      | Catch of expr * (string * expr)         (* Gestionnaire d'exception de la forme :  (exp,(str,exp1)) avec exp le contenu protégé et (str,exp1) l'abstraction en cas d'erreur *)
      | Throw                                   (* Lève une erreur *)
      
      | If of expr * expr * expr                (* Conditionnel de la forme : (expr,expr1,expr2) avec expr la condition, expr1 le "alors" et expr2 le "sinon" *)
      | Rec of string * expr                    (* Récursion de la forme : (string,expr) avec string la variable récursive et expr le corps de la fonction *)



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