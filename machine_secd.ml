open String ;;
open Printf ;;
open List ;;
open Machine_cc.CCMachine ;;
open Machine_cek.CEKMachine ;;
open Iswim.ISWIM ;;


module SECDMachine =
  struct

    (**** Types ****)

    (* Type intermédiaire pour représenter la chaîne de contrôle *)
    type c =
        Constant of int 
      | Variable of string
      | Ap
      | Prim of operateur
      | Pair of string * c list

    (* Type représentant la chaîne de contrôle *)
    type control_string = c list

    (* Type intermédiaire pour représenter l'environnement *)
    type e =  
        EnvFerm of string * (control_string * e list) 
      | EnvVar of string * int

    (* Type représentant l'environnement *)
    type env = e list

    (* Type intermédiaire pour représenter la pile *)
    type s =  
        Fermeture of (control_string * env) 
      | Stack_const of int

    (* Type représentant la pile *)
    type stack = s list

    (* Type représentant le dépôt *)
    type dump =
        Vide
      | Save of stack * env * control_string * dump

    (* Type représentant la machine SECD *)
    type secd = MachineSECD of stack * env * control_string * dump





    (**** Affichage ****)

    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
          Const const        ->   [Constant const]
            
        | Var var            ->   [Variable var]
            
        | App(expr1,expr2)   ->   append (append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Op(op,liste_expr)  ->   append (flatten( map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Abs(abs,expr)      ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]


    (* Convertit la chaîne de contrôle en une chaîne de caractère *)
    let rec string_of_control_string expression =
      match expression with
          []                       ->   ""
      
        | Constant const::t        ->   (string_of_int const)^" "^(string_of_control_string t)
      
        | Variable var::t          ->   var^" "^(string_of_control_string t)
      
        | Ap::t                    ->   "ap "^(string_of_control_string t)
      
        | Pair(abs,liste_expr)::t  ->   "< "^abs^"."^(string_of_control_string liste_expr)^"> "^(string_of_control_string t)
      
        | Prim op::t               ->   "prim "^(string_of_operateur op)^" "^(string_of_control_string t)


    (* Convertit un environnement en chaîne de caractère *)
    let rec string_of_env env =
      match env with
          []                                    ->   ""

        | [EnvFerm(var,(control_string,env))]   ->   "<"^var^" ,<"^(string_of_control_string control_string)^","^(string_of_env env)^">>"

        | EnvFerm(var,(control_string,env))::t  ->   "<"^var^" ,<"^(string_of_control_string control_string)^","^(string_of_env env)^">> , "^(string_of_env t)

        | [EnvVar(var,const)]                   ->   "<"^var^" ,"^(string_of_int const)^">"

        | EnvVar(var,const)::t                  ->   "<"^var^" ,"^(string_of_int const)^"> , "^(string_of_env t)


    (* Convertit une pile en chaîne de caractère *)
    let rec string_of_stack stack =
      match stack with
          []                                     ->   ""

        | Fermeture(control_string,env)::t       ->   "["^(string_of_control_string control_string)^" , {"^(string_of_env env)^"}] "^(string_of_stack t)

        | Stack_const b::t                       ->   (string_of_int b)^" "^(string_of_stack t)


    (* Convertit la sauvegarde en chaîne de caractère *)
    let rec string_of_dump dump =
      match dump with 
          Vide                                 ->   ""

        | Save(stack,env,control_string,dump)  ->   "( "^(string_of_stack stack)^" , ["^(string_of_env env)^"] , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )"


    (* Convertit une machine SECD en chaîne de caractère *)
    let rec string_of_secdMachine machine =
      match machine with
        MachineSECD(stack,env,control_string,dump)  ->
                                         "\n STACK   : "^(string_of_stack stack)
                                        ^"\n ENV     : ["^(string_of_env env)^"]"
                                        ^"\n CONTROL : "^(string_of_control_string control_string)
                                        ^"\n DUMP    : "^(string_of_dump dump)
                                        ^"\n"

    (* Affiche la machine SECD *)
    let afficherSECD machine = printf "MachineSECD : %s" (string_of_secdMachine machine)






    (**** Fonctions utiles ****)

    (* Substitue une variable à sa  fermeture liée *)
    let rec substitution x env =
      match env with
          []                         ->   raise AucuneSubPossible

        | EnvFerm(var,fermeture)::t  ->   if ( equal x var) then  Fermeture fermeture else substitution x t

        | EnvVar(var,b)::t           ->   if ( equal x var) then  Stack_const b else substitution x t


    (* Convertit une liste de  fermeture contenant des constante en liste d'entier *)
    let rec prendre_entier stack nbrOperande =
      match (stack,nbrOperande) with
          (t,0)                   ->   ([],t)

        | (Stack_const b::t,nbr)  ->   let (liste_entier,new_stack) = prendre_entier t (nbr-1) in (append liste_entier [b],new_stack) 
        
        | (_,_)                   ->   raise FormatOpErreur


    (* Ajoute une  fermeture à l'environnement *)
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
    let transitionSECD machine =
      match machine with
          MachineSECD(s,e,Constant b::c,d)                          ->   MachineSECD(Stack_const b::s,e,c,d)
        
        | MachineSECD(s,e,Variable x::c,d)                          ->   MachineSECD((substitution x e)::s,e,c,d)

        | MachineSECD(s,e,Pair(abs,expr)::c,d)                      ->   MachineSECD(Fermeture([Pair(abs,expr)],e)::s,e,c,d)

        | MachineSECD(s,e,Prim op::c,d)                             ->   begin
                                                                            let (liste_entier,new_stack) = prendre_entier s (getNbrOperande op) in 
                                                                            let res = (secdLanguage_of_exprISWIM (calcul op liste_entier)) in
                                                                            match res with
                                                                                [Constant b] ->  MachineSECD(Stack_const b::new_stack,e,c,d)

                                                                              | [Pair(abs,c1)] -> MachineSECD(Fermeture([Pair(abs,c1)],e)::new_stack,e,c,d)

                                                                              | _ -> raise EtatInconnu
                                                                          end

        | MachineSECD(v::Fermeture([Pair(abs,c1)],e1)::s,e,Ap::c,d)  ->   MachineSECD([],(ajoutEnv e1 abs v),c1,Save(s,e,c,d))

        | MachineSECD(v::s,e,[],Save(s1,e1,c,d))                     ->   MachineSECD(v::s1,e1,c,d)

        | _                                                          ->   raise EtatInconnu


    (* Applique les règles de la machine SECD en affichant les étapes *)
    let rec machineSECD machine afficher= 
      match machine with
          MachineSECD([Stack_const b],e,[],Vide)                ->   [Constant b]
        
        | MachineSECD([Fermeture([Pair(abs,c)],e1)],e,[],Vide)  ->   [Pair(abs,c)]

        | machine                                               ->   if (afficher) then (afficherSECD machine) else printf ""; machineSECD (transitionSECD machine) afficher

        
    (* Lance et affiche le résultat de l'expression *)
    let lancerSECD expression afficher = printf "Le résultat est %s \n" (string_of_control_string (machineSECD (MachineSECD([],[],(secdLanguage_of_exprISWIM expression),Vide)) afficher))

  end