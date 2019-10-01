open String ;;
open Printf ;;
open List ;;
open Machine_cc.CCMachine ;;
open Machine_cek.CEKMachine ;;
open Iswim.ISWIM ;;

(* Module qui implémente la machine SECD *)
module SECDMachine =
  struct

    (**** Types ****)

    (* Type intermédiaire pour représenter la chaîne de contrôle *)
    type c =
        Constant of int                    (* une constante n,m,p... *)
      | Variable of string                 (* une variable x,y,z... *)
      | Pair of string * c list            (* une abstraction lam str.(c_list) *)

      | Ap                                 (* une commande représentant l'application *)
      | Prim of operateur                  (* une commande représentant l'opération *)
      

    (* Type représentant la chaîne de contrôle *)
    type control_string = c list

    (* Type intermédiaire pour représenter l'environnement *)
    type e =  
        EnvFerm of string * (control_string * e list)  (* un élément de l'environnement composé d'une variable et d'une fermeture *)
      | EnvVar of string * int                         (* un élément de l'environnement composé d'une variable et d'une constante *)

    (* Type représentant l'environnement *)
    type env = e list

    (* Type intermédiaire pour représenter la pile *)
    type s =  
        Fermeture of (control_string * env)  (* une fermeture : pair composé d'une chaîne de contrôle et d'un environnement *)
      | Stack_const of int                   (* une constante n,m,p... *)

    (* Type représentant la pile *)
    type stack = s list

    (* Type représentant le dépôt *)
    type dump =
        Vide                                           (* élément signifiant que le dépôt est vide *)
      | Save of stack * env * control_string * dump    (* sauvegarde de la machine dans le dépôt *)

    (* Type représentant la machine SECD *)
    type secd = Machine of stack * env * control_string * dump   





    (**** Affichage ****)

    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
          Const const        ->   [Constant const]
            
        | Var var            ->   [Variable var]
            
        | App(expr1,expr2)   ->   append (append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Op(op,liste_expr)  ->   append (flatten( map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Abs(abs,expr)      ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]


    (* Convertit une liste 'a en une chaîne de caractères *)
    let rec string_of_a_list string_of_a a_list inter = 
      match a_list with 
        | []    ->   ""
        
        | [h]   ->   (string_of_a h)

        | h::t  ->   (string_of_a h)^inter^(string_of_a_list string_of_a t inter)


    (* Convertit la chaîne de contrôle en une chaîne de caractère *)
    let rec string_of_control_string expression =
      let rec string_of_cs_element element =  
        match element with
          | Constant const        ->   (string_of_int const)
        
          | Variable var          ->   var
        
          | Ap                    ->   "AP"
        
          | Pair(abs,liste_expr)  ->   "<"^abs^"."^(string_of_control_string liste_expr)^">"
        
          | Prim op               ->   "PRIM "^(string_of_operateur op)
      in string_of_a_list string_of_cs_element expression " "


    (* Convertit un environnement en chaîne de caractère *)
    let rec string_of_env env =
      let rec string_of_env_element element =
        match element with
          | EnvFerm(var,(control_string,env))   ->   "<"^var^" ,<"^(string_of_control_string control_string)^","^(string_of_env env)^">>"

          | EnvVar(var,const)                   ->   "<"^var^" ,"^(string_of_int const)^">"
      in string_of_a_list string_of_env_element env " , "


    (* Convertit une pile en chaîne de caractère *)
    let rec string_of_stack stack =
      let rec string_of_stack_element element =
        match element with
          | Fermeture(control_string,env)       ->   "["^(string_of_control_string control_string)^" , {"^(string_of_env env)^"}]"

          | Stack_const b                       ->   (string_of_int b)
      in string_of_a_list string_of_stack_element stack "  "


    (* Convertit la sauvegarde en chaîne de caractère *)
    let rec string_of_dump dump =
      match dump with 
          Vide                                 ->   ""

        | Save(stack,env,control_string,dump)  ->   "( "^(string_of_stack stack)^" , ["^(string_of_env env)^"] , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )"


    (* Convertit une machine SECD en chaîne de caractère *)
    let rec string_of_secdMachine machine =
      match machine with
        Machine(stack,env,control_string,dump)  ->
                                         "\n STACK   : "^(string_of_stack stack)
                                        ^"\n ENV     : ["^(string_of_env env)^"]"
                                        ^"\n CONTROL : "^(string_of_control_string control_string)
                                        ^"\n DUMP    : "^(string_of_dump dump)
                                        ^"\n"

    (* Affiche la machine SECD *)
    let afficherSECD machine = printf "MachineSECD : %s" (string_of_secdMachine machine)






    (**** Fonctions utiles ****)

    (* Substitue une variable à sa fermeture liée *)
    let rec substitution x env =
      match env with
          []                         ->   raise AucuneSubPossible

        | EnvFerm(var,fermeture)::t  ->   if ( equal x var) then  Fermeture fermeture else substitution x t

        | EnvVar(var,b)::t           ->   if ( equal x var) then  Stack_const b else substitution x t


    (* Convertit une liste de fermeture contenant des constantes en liste d'entiers *)
    let rec prendre_entier stack nbrOperande =
      match (stack,nbrOperande) with
          (t,0)                   ->   ([],t)

        | (Stack_const b::t,nbr)  ->   let (liste_entier,new_stack) = prendre_entier t (nbr-1) in (append liste_entier [b],new_stack) 
        
        | (_,_)                   ->   raise FormatOpErreur


    (* Ajoute une fermeture à l'environnement *)
    let rec ajoutEnv env varARemp var =
      match (env,var) with
          ([],Stack_const b)               ->   [EnvVar(varARemp,b)]

        | ([],Fermeture(c,e))              ->   [EnvFerm(varARemp,(c,e))]

        | (EnvVar(v,b)::t,Fermeture f)     ->   if (equal v varARemp) then append [EnvFerm(v,f)] t else append [EnvVar(v,b)] (ajoutEnv t varARemp (Fermeture f))

        | (EnvFerm(v,f1)::t,Fermeture f)   ->   if (equal v varARemp) then append [EnvFerm(v,f)] t else append [EnvFerm(v,f1)] (ajoutEnv t varARemp (Fermeture f))

        | (EnvVar(v,b1)::t,Stack_const b)  ->   if (equal v varARemp) then append [EnvVar(v,b)] t else append [EnvVar(v,b1)] (ajoutEnv t varARemp (Stack_const b))

        | (EnvFerm(v,f)::t,Stack_const b)  ->   if (equal v varARemp) then append [EnvVar(v,b)] t else append [EnvFerm(v,f)] (ajoutEnv t varARemp (Stack_const b))

    
                


    (**** Machine SECD ****)

    (* Applique une transition de la machine SECD pour un état donné *)
    let transition machine =
      match machine with
          Machine(s,e,Constant b::c,d)                          ->   Machine(Stack_const b::s,e,c,d)
        
        | Machine(s,e,Variable x::c,d)                          ->   Machine((substitution x e)::s,e,c,d)

        | Machine(s,e,Pair(abs,expr)::c,d)                      ->   Machine(Fermeture([Pair(abs,expr)],e)::s,e,c,d)

        | Machine(s,e,Prim op::c,d)                             ->   begin
                                                                            let (liste_entier,new_stack) = prendre_entier s (getNbrOperande op) in 
                                                                            let res = (secdLanguage_of_exprISWIM (calcul op liste_entier)) in
                                                                            match res with
                                                                                [Constant b] ->  Machine(Stack_const b::new_stack,e,c,d)

                                                                              | [Pair(abs,c1)] -> Machine(Fermeture([Pair(abs,c1)],e)::new_stack,e,c,d)

                                                                              | _ -> raise EtatInconnu
                                                                          end

        | Machine(v::Fermeture([Pair(abs,c1)],e1)::s,e,Ap::c,d)  ->   Machine([],(ajoutEnv e1 abs v),c1,Save(s,e,c,d))

        | Machine(v::s,e,[],Save(s1,e1,c,d))                     ->   Machine(v::s1,e1,c,d)

        | _                                                      ->   raise EtatInconnu


    (* Applique les règles de la machine SECD en affichant les étapes *)
    let rec machine etat afficher= 
      match etat with
          Machine([Stack_const b],_,[],Vide)               ->   [Constant b]
        
        | Machine([Fermeture([Pair(abs,c)],_)],_,[],Vide)  ->   [Pair(abs,c)]

        | indetermine                                      ->   if (afficher) then (afficherSECD indetermine) else printf ""; machine (transition indetermine) afficher

        
    (* Lance et affiche le résultat de l'expression *)
    let lancerSECD expression afficher = printf "Le résultat est %s \n" (string_of_control_string (machine (Machine([],[],(secdLanguage_of_exprISWIM expression),Vide)) afficher))

  end