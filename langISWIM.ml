open Printf ;;

module ISWIM =
  struct

    (**** Type ****)

    type exprISWIM = 
      Var of string 
      | Abs of string * exprISWIM 
      | App of exprISWIM * exprISWIM
      | Op of string * exprISWIM list
      | Const of int


     (**** Affichage ****)

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
    

    (**** Fonctions utiles ****)

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
    
    (* Vérifie l'égalité des expressions (Reste à faire l'alpha équivalence)*)
    let rec equal_expr expression1 expression2 = 

      let libreexpr1 = libre expression1 in

      let libreexpr2 = libre expression2 in

      let rec aux expr1 expr2=
        match (expr1,expr2) with

        (Var var1,Var var2) ->  
          if ((List.mem var1 (libreexpr1)) && (List.mem var2 (libreexpr2))) 
            then true 
            else (String.equal var1 var2)

        | (Const const1,Const const2) -> (const1 = const2)

        | (App(expr11,expr12),App(expr21,expr22)) -> (aux expr11 expr21) && (aux expr12 expr22) 

        | (Abs(abs1,expr1),Abs(abs2,expr2)) -> 
          if (String.equal abs1 abs2) 
            then aux expr1 expr2 
            else false

        | (Op(op1,liste_expr1),Op(op2,liste_expr2)) -> 
          if (String.equal op1 op2) 
            then 
              match (liste_expr1, liste_expr2) with
              | ([],[]) -> true
              | (h1::t1,h2::t2) -> 
                if (aux h1 h2) 
                  then (aux (Op(op1,t1)) (Op(op2,t2))) 
                  else false
              | (_,_) -> false 
            else false

        | (expr1,expr2) -> false
      in aux expression1 expression2

      (* Vérifie si l'expression est une variable *)
      let rec estVariable expr =
        match expr with
        Const const -> true
        | Var var -> true
        | Abs(abs,expr1) -> true
        | _ -> false
    
    (**** La réduction ****)

    (* Applique une réduction (A continuer) *)
    let rec reduction varARemp expr varDeRemp =
      match expr with
      Const const -> Const const

      | Var var -> 
        if (String.equal varARemp var)
          then varDeRemp
          else Var var

      | App(expr1,expr2) -> App((reduction varARemp expr1 varDeRemp),(reduction varARemp expr2 varDeRemp))

      | Op(op,liste_expr) -> Op(op,(List.map (fun x -> reduction varARemp x varDeRemp) liste_expr))

      | Abs(abs,expr) -> 
        if (String.equal abs varARemp) 
          then Abs(abs,expr)
          else  Var "x" (* A continuer *)


    (* Applique une béta réduction sur l'expression *)
    let rec beta_red expression = 
      match expression with 
      
      Var var -> Var var
      
      | Const const -> Const const

      | App(Abs(abs,expr1),expr2) -> 
        if (estVariable expr2) 
          then reduction abs expr1 expr2
          else App(Abs(abs,(beta_red expr1)),(beta_red expr2))
      
      | App(expr1,expr2) -> App((beta_red expr1),(beta_red expr2))

      | Abs(abs,expr) -> Abs(abs,(beta_red expr))

      | Op(op,liste_expr) -> Op(op,(List.map beta_red liste_expr))

  end