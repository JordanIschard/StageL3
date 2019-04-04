open Printf ;;

module ISWIM =
  struct
    type exprISWIM = 
      Var of string 
      | Abs of string * exprISWIM 
      | App of exprISWIM * exprISWIM
      | Op of string * exprISWIM list
      | Const of int
    

    (* Donne l'ensemble des variables *)
    let rec liste_variable expression =
      match expression with
      Var var -> [var]
      | Abs(abs,expr) -> liste_variable expr
      | App(expr1,expr2) -> List.append (liste_variable expr1) (liste_variable expr2)
      | Const const -> []
      | Op (op,liste_expr) -> List.flatten(List.map liste_variable liste_expr)


    (* Donne l'ensemble des variables liées de l'expression *)
    let rec lie expression =
      match expression with
        Var var -> []
       | Abs (el,expr) -> el::lie expr
       | App (expr1,expr2) -> List.append (lie expr1) (lie expr2)
       | Const const -> []
       | Op (op,liste_expr) -> List.flatten(List.map lie liste_expr)

    
    (* Donne l'ensemble des variables libres du terme *)
    let libre expression =
      let varlie = lie expression in
      let rec aux expr =
        match expr with
          Var var -> if (List.mem var varlie) then [] else [var]
         | Abs (el,expr) -> aux expr
         | App (expr1,expr2) -> List.append (aux expr1) (aux expr2)
         | Const const -> []
         | Op (op,liste_expr) -> List.flatten(List.map aux liste_expr)
      in aux expression

    

    (****AFFICHAGE****)

    (* Concatène tous les éléments d'une liste entre eux *)
    let rec concat_string_liste liste =
      match liste with
      [] -> ""
      | h::t -> h^" "^(concat_string_liste t)

    (* Affiche une expression *)
    let afficherExpr expression =
      let rec aux expr =
        match expr with 
        Var var -> var
        | Const const -> string_of_int const
        | App (expr1,expr2) -> "("^(aux expr1)^" "^(aux expr2)^")"
        | Abs (abs,expr) -> "(lambda "^abs^"."^(aux expr)^")"
        | Op (op,liste_expr) -> "("^op^" "^(concat_string_liste (List.map aux  liste_expr))^")"
      in Printf.printf "%s\n" (aux expression) 

    (* Affiche une liste de string *)
    let rec afficherList liste =
      match liste with
    [] -> printf "\n"
      | e::t -> printf "%s" e ; afficherList t

    
  end