open String ;;
open Printf ;;
open List ;;
open Cc.CCMachine ;;
open Cek.CEKMachine ;;
open LangISWIM.ISWIM ;;


module SECDMachine =
  struct

    (**** Types ****)

    type c =
      Const_C of int 
    | Var_C of string
    | Ap
    | Prim of operateur
    | Pair of string * c list


    type control_string = c list

    type e =  Env of (string * (control_string * e list))

    type env = e list

    type s =  Fermeture_secd of (control_string * env) 


    type stack = s list

    type dump =
        Vide_D
      | Save of stack * env * control_string * dump

    type secd = MachineSECD of stack * env * control_string * dump




    (**** Affichage ****)

    (* Concatène une liste de chaîne de caractère en une seule chaîne de caractère *)
    let rec concat_liste_secd liste =
      match liste with
        [] -> ""
        | h::t -> h^" "^(concat_liste_secd t)


    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
        (Const const) -> [Const_C const]
            
        | (Var var) -> [Var_C var]
            
        | (App(expr1,expr2)) ->  append (  append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | (Op(op,liste_expr)) ->  append ( flatten( map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | (Abs(abs,expr)) -> [Pair(abs,(secdLanguage_of_exprISWIM expr))]

    (* Convertit la chaîne de contrôle en une chaîne de caractère *)
    let rec string_of_control_string expression =
      match expression with
        [] -> ""
      
        | ((Const_C const)::t) -> (string_of_int const)^" "^(string_of_control_string t)
      
        | ((Var_C var)::t) -> var^" "^(string_of_control_string t)
      
        | ((Ap)::t) -> "ap "^(string_of_control_string t)
      
        | ((Pair(abs,liste_expr))::t) -> "("^abs^",("^(string_of_control_string liste_expr)^") "^(string_of_control_string t)
      
        | ((Prim(op))::t) -> "prim "^(string_of_operateur op)^" "^(string_of_control_string t)

    (* Convertit un environnement en chaîne de caractère *)
    let rec string_of_env env =
      match env with
        [] -> ""
        | (Env(var,(control_string,env)))::t ->
                    "["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_env env)^"]] , "^(string_of_env t)

    (* Convertit une pile en chaîne de caractère *)
    let rec string_of_stack stack =
      match stack with
        [] -> ""
        | ( Fermeture_secd(control_string,env))::t -> "["^(string_of_control_string control_string)^" , {"^(string_of_env env)^"}]"^(string_of_stack t)

    (* Convertit la sauvegarde en chaîne de caractère *)
    let rec string_of_dump dump =
      match dump with 
        Vide_D -> ""
        | Save(stack,env,control_string,dump) -> "( "^(string_of_stack stack)^" , "^(string_of_env env)^" , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )"

    (* Convertit une machine SECD en chaîne de caractère *)
    let rec string_of_secdMachine machine =
      match machine with
        MachineSECD(stack,env,control_string,dump) -> "( "^(string_of_stack stack)^" , "^(string_of_env env)^" , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )\n"

    (* Affiche la machine SECD *)
    let afficherSECD machine = 
       printf "MachineSECD : %s" (string_of_secdMachine machine)




    (**** Fonctions utiles ****)

    (* Substitue une variable à sa  fermeture liée *)
    let rec substitution_secd x env =
      match env with
        [] -> raise AucuneSubPossible
        | (Env(var,(control_string,env)))::t -> 
            if ( equal x var)
              then  Fermeture_secd(control_string,env)
              else substitution_secd x t

    (* Convertit une liste de  fermeture contenant des constante en liste d'entier *)
    let rec convert_liste_fermeture_secd_liste_int stack nbrOperande =
      match (stack,nbrOperande) with
        (_,0) -> []
        | (( Fermeture_secd([(Const_C b)],env))::t,nbr) -> b::(convert_liste_fermeture_secd_liste_int t (nbr - 1)) 
        | (_,_) -> raise FormatOpErreur

    (* Retire un nombre n d'élément d'une liste *)
    let rec nbrElemRetirer s nbrOperande =
      match (s,nbrOperande) with
        (s,0) -> s
        | (h::t,nbr) -> nbrElemRetirer t (nbr - 1)
        | (_,_) -> raise FormatOpErreur

    (* Vérifie si une variable est dans l'environnement *)
    let rec estDansEnvSECD env var =
      match env with
        [] -> false
        | (Env(var1,(control_string,env)))::t -> 
            if ( equal var1 var)
              then true
              else estDansEnvSECD t var 


    (* Ajoute une  fermeture à l'environnement *)
    let rec ajoutEnv_secd e1 varARemp  fermeture =
      if(estDansEnvSECD e1 varARemp)
      then e1
      else  match  fermeture with
               Fermeture_secd (control_string,env) ->  (Env(varARemp,(control_string,env)))::e1
    


              
    (**** Machine SECD ****)

    (* Applique les règles de la machine SECD en affichant les étapes *)
    let rec machineSECD machine =
      
      afficherSECD machine ; 
      match machine with

        MachineSECD(s,e,(Const_C b)::c,d) -> machineSECD (MachineSECD( ( Fermeture_secd([Const_C b] , e) :: s) , e , c , d ))

        | MachineSECD(s,e,(Var_C x)::c,d) -> machineSECD (MachineSECD( ((substitution_secd x e) :: s) , e , c , d ))

        | MachineSECD(s,e,(Prim(op))::c,d) -> 
            begin
              let nbrOperande = getNbrOperande op in 
              try machineSECD (MachineSECD (
                                (Fermeture_secd(secdLanguage_of_exprISWIM (calcul op ( rev (convert_liste_fermeture_secd_liste_int s nbrOperande))),[]))::(nbrElemRetirer s nbrOperande)
                                , e , c ,d ))
              with _ -> raise EtatInconnu
            end
        
        | MachineSECD(s,e,(Pair(abs,control_string))::c,d) -> machineSECD (MachineSECD(
                                                                            ( Fermeture_secd([Pair(abs,control_string)],e) :: s)
                                                                            , e , c , d ))
        
        | MachineSECD(v::( Fermeture_secd([Pair(abs,c1)],e1))::s,e,(Ap)::c,d) -> machineSECD (MachineSECD( [] , (ajoutEnv_secd e1 abs v) , c1 , Save(s,e,c,d) ))
                                                                          
        | MachineSECD(v::s,e,[],Save(s1,e1,c,d)) -> machineSECD (MachineSECD( v::s1 , e1 , c , d ))

        | MachineSECD(( Fermeture_secd([Const_C const],env))::s,e,[],Vide_D) -> [Const_C const]

        | MachineSECD(( Fermeture_secd([Pair(abs,c)],env))::s,e,[],Vide_D) -> [Pair(abs,c)]

        | _-> raise EtatInconnu

    (* Lance et affiche le résultat de l'expression *)
    let lancerSECD expression =
       printf "Le résultat est %s \n" (string_of_control_string (machineSECD (MachineSECD([],[],(secdLanguage_of_exprISWIM expression),Vide_D))))
    
  end