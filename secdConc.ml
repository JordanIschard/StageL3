open String ;;
open Printf ;;
open List ;;
open LangISWIMConc.ISWIM ;;


module SECDMachine =
  struct

    (**** Types ****)

    type id = string

    type c =
      Const_C of int                                (* constante b *)
    | Var_C of string                               (* variable X *)
    | Ap                                            (* application *)
    | Prim of operateur                             (* opérateur *)
    | Pair of string * c list                       (* abstraction *)
    | Bspawn                                        (* début du spawn *)
    | Espawn                                        (* fin du spawn *)
    | Emit_SECD of id                               (* emet s *)
    | Present_SECD of id * c list * c list          (* present s in t1 t2 *)
    | Signal_SECD of id * c list                    (* signal s in t *)

    

    type control_string = c list

    type e =  
       Env of (string * (control_string * e list))  (* (s,(C,Env)) *)
      | Bind of id                                  (* (init,s) *)
      | Emit_Env of id                              (* (emit,s) *)

    type env = e list

    type s =  
        Remp                                        (* remplace *)
      | Fermeture_secd of (control_string * env)    (* fermeture (C,Env) *)

    type stack = s list


    type dump =
        Vide_D
      | Save of stack * env * control_string * dump

    type wait = dump list

    type st = id * dump

    type stuck = st list

    type secd = MachineSECD of stack * env * control_string * wait * stuck * dump

    exception AucuneSubPossible
    exception EtatInconnu
    exception FinEtrange
    exception FinSpawnNonTrouvable
    exception SignalDejaInit
    exception SignalDejaEmit
    exception SignalNonInit



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

        | (Spawn expr) -> append [Bspawn] (append (secdLanguage_of_exprISWIM expr) [Espawn])

        | Present (signal,expr1,expr2) -> [Present_SECD (signal,(secdLanguage_of_exprISWIM expr1),(secdLanguage_of_exprISWIM expr2))]

        | Emit (signal) -> [Emit_SECD (signal)]

        | Signal (signal,expr) -> [Signal_SECD (signal,(secdLanguage_of_exprISWIM expr))]

    (* Convertit la chaîne de contrôle en une chaîne de caractère *)
    let rec string_of_control_string expression =
      match expression with
        [] -> ""
      
        | ((Const_C const)::t) -> (string_of_int const)^" "^(string_of_control_string t)
      
        | ((Var_C var)::t) -> var^" "^(string_of_control_string t)
      
        | ((Ap)::t) -> "ap "^(string_of_control_string t)
      
        | ((Pair(abs,liste_expr))::t) -> "("^abs^",("^(string_of_control_string liste_expr)^") "^(string_of_control_string t)
      
        | ((Prim(op))::t) -> "prim "^(string_of_operateur op)^" "^(string_of_control_string t)

        | ((Bspawn)::t) -> "bspawn "^(string_of_control_string t)

        | ((Espawn)::t) -> " espawn "^(string_of_control_string t)

        | ((Present_SECD(signal,expr1,expr2))::t) -> " present "^signal^" in "^(string_of_control_string expr1)^" "^(string_of_control_string expr2)^" "^(string_of_control_string t)

        | ((Emit_SECD signal)::t) -> "emit "^signal^" "^(string_of_control_string t)

        | ((Signal_SECD(signal,expr))::t) -> "signal "^signal^" in "^(string_of_control_string expr)^" "^(string_of_control_string t)

    (* Convertit un environnement en chaîne de caractère *)
    let rec string_of_env env =
      match env with
        [] -> ""
        | (Env(var,(control_string,env)))::t ->
                    "["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_env env)^"]] , "^(string_of_env t)
        | (Emit_Env(signal))::t -> "[emit,"^signal^"] , "^(string_of_env t)
       
        | (Bind(signal))::t -> "[init,"^signal^"] , "^(string_of_env t)

    (* Convertit une pile en chaîne de caractère *)
    let rec string_of_stack stack =
      match stack with
        [] -> ""
        | ( Fermeture_secd(control_string,env))::t -> "["^(string_of_control_string control_string)^" , {"^(string_of_env env)^"}]"^(string_of_stack t)
        | Remp::t -> "Remp"^(string_of_stack t)

    (* Convertit la sauvegarde en chaîne de caractère *)
    let rec string_of_dump dump =
      match dump with 
        Vide_D -> ""
        | Save(stack,env,control_string,dump) -> "( "^(string_of_stack stack)^" , "^(string_of_env env)^" , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )"

    let rec string_of_wait wait =
      match wait with 
        [] -> ""
        | dump::t -> "("^(string_of_dump dump)^") , "^(string_of_wait t)

    let rec string_of_stuck stuck =
      match stuck with 
        [] -> ""
        | (signal,dump)::t -> "( "^signal^" , "^(string_of_dump dump)^") , "^(string_of_stuck t)

    (* Convertit une machine SECD en chaîne de caractère *)
    let rec string_of_secdMachine machine =
      match machine with
        MachineSECD(stack,env,control_string,wait,stuck,dump) -> "( "^(string_of_stack stack)^" , "^(string_of_env env)^
        " , "^(string_of_control_string control_string)^" , "^(string_of_wait wait)^" , "^(string_of_stuck stuck)^" , "^(string_of_dump dump)^" )\n"

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
        | _::t -> substitution_secd x t 

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
        | _::t -> estDansEnvSECD t var

    (* Vérifie si c'est un init *)
    let rec estBind env signal =
      match env with
       [] -> false
        | (Bind(signal1))::t -> if (equal signal signal1) then true else estBind t signal
        | _::t -> estBind t signal

    (* Vérifie si c'est une émission *)
    let rec estEmit env signal =
      match env with
        [] -> false
        | (Emit_Env(signal1))::t -> if (equal signal signal1) then true else estEmit t signal
        | _::t -> estEmit t signal

    (* Ajoute une  fermeture à l'environnement *)
    let rec ajoutEnv_secd env varARemp  fermeture =
      if(estDansEnvSECD env varARemp)
      then env
      else  match  fermeture with
               Fermeture_secd (control_string,env1) ->  (Env(varARemp,(control_string,env1)))::env
              | Remp -> raise EtatInconnu

    (* Ajoute un signal initialisé dans l'environnement *)
    let rec ajoutBind env signal = 
      if(estBind env signal)
        then raise SignalDejaInit
        else (Bind(signal))::env
    
    (* Ajoute une émission dans l'environnement *)
    let rec ajoutEmit env signal = 
      if(estEmit env signal)
        then raise SignalDejaEmit
        else (Emit_Env(signal))::env
    
    (* Retire la partie du spawn du control string *)
    let rec spawnRetirer control_string =
      match control_string with
        [] -> raise FinSpawnNonTrouvable
        | Espawn::t -> t
        | h::t -> spawnRetirer t

    (* Récupère la partie du spawn du control string *)
    let rec spawnRecup control_string =
      match control_string with
        [] -> raise FinSpawnNonTrouvable
        | Espawn::t -> []
        | h::t -> h::(spawnRecup t)

    (* Retire les emissions de la liste d'environnement *)
    let rec emitRetirer env =
      match env with
        [] -> []
        | (Emit_Env(signal))::t -> emitRetirer t
        | h::t -> h::(emitRetirer t)
 
    (* Dans le cas où le signal attendu n'est pas emit on applique le second choix*)
    let rec secondChoix st =
      match st with
        [] -> []
        | (signal,Save(s,e,((Present_SECD(signal1,c1,c2))::c),d))::t -> (Save( s , (emitRetirer e) , (append c2 c) , d ))::(secondChoix t)
        | _ -> raise EtatInconnu

    (* Donne la liste des éléments de stuck qui réagisse au signal*)
    let rec reveil signal stuck =
      match stuck with
       [] -> []
       | (signal1,save)::t -> 
          if (equal signal signal1) 
            then save::(reveil signal t)
            else (reveil signal t)

    (* Donne la liste des éléments de stuck qui ne réagisse pas au signal *)
    let rec resteStuck signal stuck =
      match stuck with
        [] -> []
        | (signal1,save)::t -> 
        if (equal signal signal1) 
          then (resteStuck signal t)
          else (signal1,save)::(resteStuck signal t)

    (**** Machine SECD ****)

    (* Applique les règles de la machine SECD en affichant les étapes *)
    let rec machineSECD machine =
      
      afficherSECD machine ; 
      match machine with

        MachineSECD(s,e,(Const_C b)::c,w,st,d) -> machineSECD (MachineSECD( ( Fermeture_secd([Const_C b] , e) :: s) , e , c , w , st , d ))

        | MachineSECD(s,e,(Var_C x)::c,w,st,d) -> machineSECD (MachineSECD( ((substitution_secd x e) :: s) , e , c , w , st , d ))

        | MachineSECD(s,e,(Prim(op))::c,w,st,d) ->
            begin
              let nbrOperande = getNbrOperande op in 
              try machineSECD (MachineSECD (
                                (Fermeture_secd(secdLanguage_of_exprISWIM (calcul op ( rev (convert_liste_fermeture_secd_liste_int s nbrOperande))),[]))::(nbrElemRetirer s nbrOperande)
                                , e , c , w , st , d ))
              with _ -> raise EtatInconnu
            end
        
        | MachineSECD(s,e,(Pair(abs,control_string))::c,w,st,d) -> machineSECD (MachineSECD(
                                                                            ( Fermeture_secd([Pair(abs,control_string)],e) :: s)
                                                                            , e , c , w , st , d ))

        | MachineSECD(v::Remp::s,e,(Ap)::c,w,st,d) -> machineSECD (MachineSECD(v::s,e,c,w,st,d))

        | MachineSECD(Remp::v::s,e,(Ap)::c,w,st,d) -> machineSECD (MachineSECD(v::s,e,c,w,st,d))

        | MachineSECD(v::( Fermeture_secd([Pair(abs,c1)],e1))::s,e,(Ap)::c,w,st,d) -> machineSECD (MachineSECD( [] , (ajoutEnv_secd e1 abs v) , c1 , w , st , Save(s,e,c,d) ))

        | MachineSECD(v::s,e,[], w , st ,Save(s1,e1,c,d)) -> machineSECD (MachineSECD( v::s1 , e1 , c , w , st , d ))

        | MachineSECD(s,e,(Bspawn)::c,w,st,d) -> machineSECD (MachineSECD( Remp::s , e , (spawnRetirer c) , (append w [(Save(s,e,(spawnRecup c),d))] ), st , d ))

        | MachineSECD(s,e,(Signal_SECD(signal,c1))::c,w,st,d) -> machineSECD (MachineSECD( [] , (ajoutBind e signal) , c1 , w , st , Save(s,e,c,d)))

        | MachineSECD(s,e,(Present_SECD(signal,c1,c2))::c,w,st,d) ->
          if (estBind e signal)
            then 
              if (estEmit e signal)
                then machineSECD (MachineSECD( s , e ,(append c1 c) , w , st , d ))

                else 
                  match w with
                    [] -> machineSECD (MachineSECD( [] , [] , [] , [] , (append st [(signal,(Save(s,e,(Present_SECD(signal,c1,c2))::c,d)))]) , Vide_D ))
                    | (Save(s,e,c,d))::t -> machineSECD (MachineSECD( s , e , c , t , (append st [(signal,(Save(s,e,(Present_SECD(signal,c1,c2))::c,d)))]) , Vide_D ))
                    | _ -> raise EtatInconnu

            else raise SignalNonInit

        | MachineSECD([],[],[],(Save(s,e,c,d))::w,st,Vide_D) -> machineSECD (MachineSECD( s , e , c , w , st , d ))

        | MachineSECD(s,e,(Emit_SECD(signal))::c,w,st,d) -> machineSECD (MachineSECD( Remp::s , (ajoutEmit e signal) , c , (append (reveil signal st) w) , (resteStuck signal st) , d ))
        
        | MachineSECD([],[],[],[],st,Vide_D) -> machineSECD (MachineSECD( [] , [] , [] , (secondChoix st) , [] , Vide_D ))

        | MachineSECD(( Fermeture_secd([Const_C const],env))::s,e,[],[],[],Vide_D) -> [Const_C const]

        | MachineSECD(Remp::s,e,[],w,st,Vide_D) -> raise FinEtrange

        | MachineSECD(( Fermeture_secd([Pair(abs,c)],env))::s,e,[],[],[],Vide_D) -> [Pair(abs,c)]

        | _-> raise EtatInconnu

    (* Lance et affiche le résultat de l'expression *)
    let lancerSECD expression =
       printf "Le résultat est %s \n" (string_of_control_string (machineSECD (MachineSECD([],[],(secdLanguage_of_exprISWIM expression),[],[],Vide_D))))
    
  end