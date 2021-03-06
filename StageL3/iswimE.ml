open Printf ;;
open String ;;
open List ;;

(* Module qui implémente le langage ISWIM avec les erreurs *)
module ISWIM =
  struct

    (**** Types ****)
    
     (* Type représentant les opérateurs reconnus par le langage *)
     type operateur = 
      Add1           (* ++   *)
    | Sub1           (* --   *)
    | IsZero         (* == 0 *)
    | Add            (* +    *)
    | Sub            (* -    *)
    | Mult           (* *    *)
    | Div            (* /    *)


  (* Type représentant les éléments qui constitue le langage *)
  type expr = 
      Var of string                     (* une variable x,y,z,...         *)       
    | Abs of string * expr              (* une abstraction lam x.t        *)
    | App of expr * expr                (* une application ( a b )        *)
    | Op of operateur * expr list       (* une opération [op ; el1 ; ...] *)
    | Const of int                      (* une constante a,b,c,...        *)
    | Erreur of string                  (* une erreur                     *)










    (***** Exception *****)

    exception EquivalenceImpossible
    exception FormatOpErreur
    exception OpInvalide
    exception NotConstErreur










    (**** Affichage ****)

    (* Concatène tous les éléments d'une liste entre eux *)
    let rec concat_string_liste liste =
      match liste with
          []    ->   ""

        | h::t  ->   h^" "^(concat_string_liste t)


    (* Convertit un opérateur en chaîne de caractères *)
    let string_of_operateur op =
      match op with
          Add1    ->   "++"
        | Sub1    ->   "--"
        | IsZero  ->   "== 0"
        | Add     ->   "+"
        | Sub     ->   "-"
        | Mult    ->   "*"
        | Div     ->   "/"
      

    (* Convertit une expression en chaîne de caractères *)
    let rec string_of_expr expr =
      match expr with 
          Var var             ->   var
       
        | Const const         ->   string_of_int const
       
        | App (expr1,expr2)   ->   "("^(string_of_expr expr1)^" "^(string_of_expr expr2)^")"
       
        | Abs (abs,expr)      ->   "(lam "^abs^"."^(string_of_expr expr)^")"
       
        | Op (op,liste_expr)  ->   "("^(string_of_operateur op)^" "^(concat_string_liste ( map string_of_expr  liste_expr))^")"
       
        | Erreur erreur        ->   erreur


    (* Affiche une expression *)
    let afficherExpr expression = printf "%s\n" (string_of_expr expression) 
    

    (* Affiche une liste de pairs de chaîne de caractères *)
    let rec afficherPairList liste =
      match liste with
          []          ->   printf "\n"

        | (e1,e2)::t  ->   printf " (%s,%s)  " e1 e2 ; afficherPairList t
    









    (**** Fonctions utiles ****)


    (* Donne le nombre d'opérande requis pour utiliser l'opérateur *)
    let getNbrOperande op =
      match op with
          Add1 | Sub1 | IsZero    ->   1

        | Add | Sub | Mult | Div  ->   2


    (* Donne l'ensemble des variables *)
    let rec liste_variable expression =
      match expression with
          Var var            ->   [var]

        | Abs(_,expr)        ->   liste_variable expr

        | App(expr1,expr2)   ->   append (liste_variable expr1) (liste_variable expr2)

        | Const _        ->   []

        | Op(op,liste_expr)  ->   flatten( map liste_variable liste_expr)

        | Erreur _      ->   []


    (* Donne l'ensemble des variables liées de l'expression *)
    let rec lie expression =
      match expression with
          Var _              ->   []
 
        | Abs(el,expr)       ->   el::lie expr

        | App(expr1,expr2)   ->   append (lie expr1) (lie expr2)

        | Const _            ->   []

        | Op(_,liste_expr)   ->   flatten( map lie liste_expr)

        | Erreur _           ->   []

    
    (* Donne l'ensemble des variables libres de l'expression *)
    let libre expression =
      let varlie = lie expression in
      let rec aux expr =
        match expr with
            Var var            ->   if (mem var varlie) then [] else [var]

          | Abs(_,expr)        ->   aux expr

          | App(expr1,expr2)   ->   append (aux expr1) (aux expr2)

          | Const _            ->   []

          | Op(_,liste_expr)   ->   flatten( map aux liste_expr)

          | Erreur _           ->   []
      in 
      aux expression


      (* Vérifie si l'expression est une variable *)
      let estVariable expr =
        match expr with
            Const _ | Var _ | Abs _  ->   true

          | _         ->   false


      (* Vérifie si l'expression est une constante *)
      let estConst expr =
        match expr with
            Const _  ->   true
          | _        ->   false


      (* Vérifie si l'expression est une erreur *)
      let estErreur expr =
        match expr with
            Erreur _  ->   true
          | _         ->   false


      (* Convertit une liste de constantes en liste d'entiers et lève une exception si la liste ne contient pas seulement des constantes *)
      let rec convert_liste_expr_liste_int liste =
        match liste with
            []              ->   []

          | Const const::t  ->   const::(convert_liste_expr_liste_int t)

          | _               ->   raise NotConstErreur


      (* Vérifie si l'élément fait partie de, au moins, un couple de la liste *)
      let rec estDansUnCouple elem liste =
        match liste with
            []          ->   false

          | (h1,h2)::t  ->   if ( equal elem h1) then true else 
                                                            if ( equal elem h2) then true else estDansUnCouple elem t
      









    (**** La réduction ****)


    (* Donne un nouveau nom de variables à partir d'une liste finie *)
    let renommage liste_interdit =
      let variables = ["x";"y";"z";"w"] in
      let rec aux liste_interdit variables =
        match variables with
          | []    ->   "lkihgoihoi"

          | h::t  ->   if (mem h liste_interdit) then aux liste_interdit t else h
      in
      aux liste_interdit variables


    (* Renomme si nécessaire *)
    let renommer abs expr varARemp varDeRemp =
      let variables = liste_variable varDeRemp
      in
      let rec aux abs = 
        if(mem abs variables) then aux (renommage variables) else abs
      in 
      aux abs

    (* Applique une réduction *)
    let rec reduction varARemp expr varDeRemp =
      match expr with
          Const const        ->   Const const

        | Var var            ->   if ( equal varARemp var) then varDeRemp else Var var

        | App(expr1,expr2)   ->   App((reduction varARemp expr1 varDeRemp),(reduction varARemp expr2 varDeRemp))

        | Op(op,liste_expr)  ->   Op(op,( map (fun x -> reduction varARemp x varDeRemp) liste_expr))

        | Abs(abs,expr)      ->   if ( equal abs varARemp) then Abs(abs,expr)
                                                           else 
                                                             let newX = (renommer abs expr varARemp varDeRemp) in
                                                             Abs(newX,(reduction varARemp (reduction abs expr (Var newX)) varDeRemp))

        | Erreur erreur      -> Erreur erreur 


    (* Applique une béta réduction sur l'expression *)
    let rec beta_red expression = 
      match expression with 
          Var var            ->   Var var
        
        | Const const        ->   Const const
        
        | App(expr1,expr2)   -> 
            begin
              match (expr1,expr2) with
                  (Erreur erreur,_)               ->   Erreur("Erreur : "^erreur)

                | (_,Erreur erreur)               ->   Erreur("Erreur : "^erreur) 

                | (Const const,_)                 ->   Erreur("La constante "^(string_of_int const)^" ne peut pas être utilisé")

                | (Abs(abs,Erreur erreur),expr2)  ->   Erreur("Erreur :"^erreur)

                | (Abs(abs,expr1),expr2)          ->   if (estVariable expr2) then (reduction abs expr1 expr2) else  (App(Abs(abs,(beta_red expr1)),(beta_red expr2)))

                | (_,_)                           ->   App((beta_red expr1),(beta_red expr2))
            end

        | Abs(abs,expr)      ->   Abs(abs,(beta_red expr))

        | Op(op,liste_expr)  ->   Op(op,(map beta_red liste_expr))

        | Erreur erreur      ->   Erreur erreur


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


      (* Applique une delta réduction sur l'expression *)
      let rec  delta_red expression =
        let rec aux liste =
          match liste with
              []           ->   false

            | Abs _::t  ->   true

            | _::t         ->   aux t
        in
        match expression with 
            Var var             ->   Var var 

          | Const const         ->   Const const

          | App(expr1,expr2)    ->   App((delta_red expr1),(delta_red expr2))

          | Abs(abs,expr)       ->   Abs(abs,(delta_red expr))

          | Op(op, liste_expr)  -> 
              if ( for_all estConst liste_expr) 
                then if ((getNbrOperande op) = (length liste_expr) ) then (calcul op (convert_liste_expr_liste_int liste_expr)) else Erreur "Nombre d'élément Invalide"     
                else if (aux liste_expr) then Erreur "Une abstraction est contenu dans la liste de l'opération" else Op(op,( map delta_red liste_expr))
          
            | Erreur erreur     ->   Erreur erreur


      (* Créer une liste de pairs qui correspond aux variables liées qui doivent être obligatoirement placées au même endroit *)
      let rec pre_alpha_eq expression1 expression2 =
        match (expression1,expression2) with
          (Var _,Var _)                              ->   []
        
        | (Const _,Const _)                          ->   []

        | (App(expr1 ,expr2),App(expr3,expr4))       ->   append (pre_alpha_eq expr1 expr3) (pre_alpha_eq expr2 expr4)

        | (Abs(abs1,expr1),Abs(abs2,expr2))          ->   append [(abs1,abs2)] (pre_alpha_eq expr1 expr2)

        | (Op(op1,liste_expr1),Op(op2,liste_expr2))  -> 
              begin
                match (liste_expr1,liste_expr2) with
                    ([],[])          ->   []

                  | (h1::t1,h2::t2)  ->   append (pre_alpha_eq h1 h2) (pre_alpha_eq (Op(op1,t1)) (Op(op2,t2))) 

                  | _                ->   raise EquivalenceImpossible
              end

        | (_,_)                                      ->   raise EquivalenceImpossible


      (* Teste les deux expressions avec les règles de l'apha équivalence *)
      let alpha_eq liste_pair_lie expression1 expression2 =
        let rec aux expr1 expr2 =
          match (expr1,expr2) with
              (Const const1,Const const2)                ->   (const1 = const2)
            
            | (Var var1 , Var var2)                      ->   if ((estDansUnCouple var1 liste_pair_lie) || (estDansUnCouple var2 liste_pair_lie)) 
                                                                then ( mem (var1,var2) liste_pair_lie) 
                                                                else true
            
            | (App(expr1,expr2),App(expr3,expr4))        ->   (aux expr1 expr3) && (aux expr2 expr4)

            | (Abs(_,expr1),Abs(_,expr2))                ->   (aux expr1 expr2)

            | (Op(op1,liste_expr1),Op(op2,liste_expr2))  -> 
                  begin
                    match (liste_expr1,liste_expr2) with
                        ([],[])            ->   true
                      
                      | (h1::t1 , h2::t2)  ->   if (aux h1 h2) then aux (Op(op1,t1)) (Op(op2,t2)) else false   

                      | _                  ->   raise EquivalenceImpossible
                  end
            | (_,_)                                      ->   false
        in 
        aux expression1 expression2  


      (* Verifie si il y a une alpha équivalence entre deux expressions *)
      let equalExpr expression1 expression2 =
        try   let liste_pair = pre_alpha_eq expression1 expression2 in alpha_eq liste_pair expression1 expression2
        with  EquivalenceImpossible -> false 


      (* Réduit en forme normale si c'est possible *)
      let rec n_red expression =
        afficherExpr expression;
        let newExpr = (delta_red (beta_red expression)) in if (equalExpr expression newExpr || estErreur newExpr) then Printf.printf "->  %s" (string_of_expr newExpr) else n_red newExpr
        
  end