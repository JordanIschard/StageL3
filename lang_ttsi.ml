open Printf ;;
open String ;;
open List ;;

(* Module qui implémente le langage ISWIM *)
module ISWIM =
  struct

    (**** Types ****)
    
    (* Opérateur du langage *)
    type operateur = 
      | Add1 
      | Sub1
      | IsZero
      | Add
      | Sub
      | Mult
      | Div 
      | Egal


    (* pattern utilisé pour faire du filtrage *)
    type pat = 
      | Variable of string                        (* on filtre avec une variable *)
      | Pattern of int * pat list                 (* on filtre avec un pattern *)
      | Neutral


    (* Expression du langage *)
    type expr = 

      | Var   of string                         (* une variable *)
      | Const of int                            (* une constante *)
      
      | Abs of string * expr                    (* Abstraction de la forme : lam string.(expr) *)
      | App of expr * expr                      (* Application de la forme : (expr expr1)        *)
      | Op  of operateur * expr list            (* Opération de la forme : (operateur expr_list) avec expr_list une liste de taille égal au nombre de variable de l'opérateur *)
      
      | Spawn   of expr                         (* Création thread de la forme : (expr) avec expr la partie de la chaîne de contrôle que l'on va prendre pour le thread créé *)
      | Present of string * expr * expr         (* Conditionnel pour un signal de la forme : (string,expr,expr1) avec string le signal, expr la partie prise si le signal est émis et expr1 la partie prise si le signal n'est pas émis *)
      | Emit    of string                       (* Emission de la forme : (string) avec string le signal *)
      | Signal                                  (* Initialise *)
      | Wait                                    (* Met en attente *)

      | Put of string * int                     (* Ajoute une valeur de la forme : (string,int) avec string le signal et int la valeur à ajouter *)
      | Get of string * string * int            (* Prend une valeur de la forme : (string,string1,int) avec string le signal, string1 le thread et int l'élément neutre *)

      | Rec of string * expr                    (* Récursion de la forme : (string,expr) avec string la variable récursive et expr le corps de la fonction *)
      | If  of expr * expr * expr               (* Conditionnel de la forme : (expr,expr1,expr2) avec expr la condition, expr1 le "alors" et expr2 le "sinon" *)

      | Build of int * int * expr list          (* Build de la forme : (int,int1,expr_list) avec int l'id du constructeur, int1 le nombre de paramètre et expr_list une liste de taille égal au nombre de paramètre *)
      | Match of string * ( pat * expr ) list   (* Match de la forme : (string,(pattern,expr) list) avec string l'element à matcher, list une liste de filtres composées d'un pattern et une expr *)

      | Catch of expr * (string * expr)
      | Raise of string


    (* Vraie en lambda-calcul *)
    let vraie = Abs("x",Abs("y",App(Var "x",Const 1)))


    (* Faux en lambda-calcul *)
    let faux = Abs("x",Abs("y",App(Var "y",Const 1)))


    exception DivByZero
    exception InvalidOpFormat


    (* Convertit un opérateur en chaîne de caractère *)
    let string_of_operateur op =
      match op with
        | Add1       ->   "++"

        | Sub1       ->   "--"

        | IsZero     ->   "== 0"

        | Add        ->   "+"

        | Sub        ->   "-"

        | Mult       ->   "*"

        | Div        ->   "/"

        | Egal       ->   "="


    (* Renvoie le nombre d'opérande pour un opérateur donné *)
    let nbr_operande op =
      match op with
        | Add1       ->   1

        | Sub1       ->   1

        | IsZero     ->   1

        | Add        ->   2

        | Sub        ->   2

        | Mult       ->   2

        | Div        ->   2

        | Egal       ->   2


    (* Renvoie le résultat d'un calcul *)
    let calculer op operandes =
      match (op,operandes) with
        | (Add1,[n])      ->   Const (n+1)

        | (Sub1,[n])      ->   Const (n-1)

        | (IsZero,[n])    ->   if n=0 then vraie else faux

        | (Add,[n;m])     ->   Const (n+m)

        | (Sub,[n;m])     ->   Const (n-m)

        | (Mult,[n;m])    ->   Const (n*m)

        | (Div,[n;m])     ->   if m = 0 then raise DivByZero else Const (n/m)

        | (Egal,[n;m])    ->   if n = m then vraie else faux

        | _               ->   raise InvalidOpFormat


  end