open String ;;
open Printf ;;
open List ;;
open LangISWIMConcE.ISWIM ;;


module SECDMachine =
  struct

    (**** Types ****)

    type id = string
    type variable = string

    type c =
      Const_C of int                                (* constante b *)
    | Var_C of variable                             (* variable X *)
    | Ap                                            (* application *)
    | Prim of operateur                             (* opérateur *)
    | Pair of variable * c list                     (* abstraction *)
    | Bspawn                                        (* début du spawn *)
    | Espawn                                        (* fin du spawn *)
    | Emit_SECD of id                               (* emet s *)
    | Present_SECD of id * c list * c list          (* present s in t1 t2 *)
    | Signal_SECD of id * c list                    (* signal s in t *)
    | Throw_SECD of int
    | Catch_SECD of int * c list * c list 

    type control_string = c list


    type e =  
       Env of (variable * (control_string * e list)) (* (s,(C,Env)) *)
      | Init of id                                   (* (init,s) *)

    type env = e list


    type s =  
        Remp                                        (* remplace *)
      | Fermeture_secd of (control_string * env)    (* fermeture (C,Env) *)

    type stack = s list


    type signal = Emit_SI of id                     (* (emit,s) *)

    type si = signal list


    type dump =
        Vide_D
      | Save of stack * env * control_string * dump


    type wait = dump list


    type st = id * dump


    type stuck = st list

    type handler = 
        Vide_H
      | SaveHandler of int * (stack * env * control_string * wait * stuck * si * dump * handler) 


    type secd = MachineSECD of stack * env * control_string * wait * stuck * si * dump * handler 




    (**** Exception ****)

    exception AucuneSubPossible
    exception EtatInconnu
    exception FinEtrange
    exception FinSpawnNonTrouvable
    exception SignalDejaInit
    exception SignalDejaEmit
    exception SignalNonInit
    exception EtatDumpInconnu
    exception EtatEnvInvalide
    exception EtatStuckInvalide
    exception FormatWaitInvalide




    (**** Affichage ****)

    (* Concatène une liste de chaîne de caractère en une seule chaîne de caractère *)
    let rec concat_liste_secd liste =
      match liste with
        [] -> "" | h::t -> h^" "^(concat_liste_secd t)


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

        | (Throw erreur) -> [Throw_SECD(erreur)]

        | (Catch(erreur,expr1,expr2)) -> [Catch_SECD(erreur,(secdLanguage_of_exprISWIM expr1),(secdLanguage_of_exprISWIM expr1))]


    let message_of_erreur erreur =
      match erreur with
        1 -> "ERREUR : Je ne comprends pas ce qu'il s'est passé :/"
        | 2 ->  "ERREUR : Format de la liste bloqué invalide"
        | 3 ->  "ERREUR : Format de la liste d'attente invalide"
        | 4 ->  "ERREUR : Format de l'environnement invalide"
        | 5 ->  "ERREUR : Format de l'opérateur erroné"
        | 6 ->  "ERREUR : Le spawn n'est pas bien délimité"
        | 7 ->  "ERREUR : Signal non initialisé"
        | 8 ->  "ERREUR : Signal déjà initialisé"
        | _ ->  "ERREUR : Cette erreur n'existe pas "

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

        | ((Throw_SECD(erreur))::t) -> (message_of_erreur erreur)^" "^(string_of_control_string t)

        | ((Catch_SECD(erreur,expr1,expr2))::t) -> "try "^(string_of_control_string expr1)^" catch "^(message_of_erreur erreur)^" in ("^(string_of_control_string expr2)^")"^(string_of_control_string t) 

    (* Convertit un environnement en chaîne de caractère *)
    let rec string_of_env env =
      match env with
        [] -> ""
        | (Env(var,(control_string,env)))::t -> "["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_env env)^"]] , "^(string_of_env t)
        | (Init(signal))::t -> "[init,"^signal^"] , "^(string_of_env t)

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

    (* Convertit la liste des éléments en attente en chaîne de caractère *)
    let rec string_of_wait wait =
      match wait with 
        [] -> "" | dump::t -> "("^(string_of_dump dump)^") , "^(string_of_wait t)

    (* Convertit la liste des éléments bloqués en chaîne de caractère *)
    let rec string_of_stuck stuck =
      match stuck with 
        [] -> "" | (signal,dump)::t -> "( "^signal^", "^(string_of_dump dump)^" , "^(string_of_stuck t)

    (* Convertit la liste des signaux émits en chaîne de caractère *)
    let rec string_of_si si =
      match si with
        [] -> "" | (Emit_SI(signal))::t -> "[emit,"^signal^"] , "^(string_of_si t) 

    let rec string_of_handler handler =
          match handler with
            Vide_H -> ""
            | SaveHandler(erreur,(stack,env,control_string,wait,stuck,si,dump,hand)) -> 
            "\n     STACK   : "^(string_of_stack stack)^" \n    ENV     : "^(string_of_env env)^" \n    CONTROL : "^(string_of_control_string control_string)^" \n"^
            "     WAIT    : "^(string_of_wait wait)^" \n    STUCK   : "^(string_of_stuck stuck)^" \n    SI      : "^(string_of_si si)^" \n    DUMP    : "^(string_of_dump dump)^" \n"^
            " HANDLER    : "^(string_of_handler hand)^" \n"

    (* Convertit une machine SECD en chaîne de caractère *)
    let rec string_of_secdMachine machine =
      match machine with
        MachineSECD(stack,env,control_string,wait,stuck,si,dump,handler) -> 
        "\n STACK   : "^(string_of_stack stack)^" \n ENV     : "^(string_of_env env)^" \n CONTROL : "^(string_of_control_string control_string)^" \n"^
        " WAIT    : "^(string_of_wait wait)^" \n STUCK   : "^(string_of_stuck stuck)^" \n SI      : "^(string_of_si si)^" \n DUMP    : "^(string_of_dump dump)^" \n"^
        " HANDLER : "^(string_of_handler handler)^" \n"

    (* Affiche la machine SECD *)
    let afficherSECD machine = 
       printf "MachineSECD : %s\n" (string_of_secdMachine machine)




    (**** Fonctions utiles ****)

    (* Substitue une variable à sa  fermeture liée *)
    let rec substitution_secd x env =
      match env with
        [] -> raise AucuneSubPossible
        | (Env(var,(control_string,env)))::t -> if (equal x var) then  Fermeture_secd(control_string,env) else substitution_secd x t
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
        | (Env(var1,(control_string,env)))::t -> if ( equal var1 var) then true else estDansEnvSECD t var 
        | _::t -> estDansEnvSECD t var

    (* Vérifie si c'est un init *)
    let rec estInit env signal =
      match env with
       [] -> false
        | (Init(signal1))::t -> if (equal signal signal1) then true else estInit t signal
        | _::t -> estInit t signal

    (* Vérifie si c'est une émission *)
    let rec estEmit si signal =
      match si with
        [] -> false
        | (Emit_SI(signal1))::t -> if (equal signal signal1) then true else estEmit t signal


    (* Ajoute une fermeture à l'environnement *)
    let rec ajoutEnv_secd env varARemp  fermeture =
      if(estDansEnvSECD env varARemp) then env
      else  match  fermeture with
               Fermeture_secd (control_string,env1) ->  (Env(varARemp,(control_string,env1)))::env
              | Remp -> raise EtatEnvInvalide

    (* Ajoute un signal initialisé dans l'environnement *)
    let rec ajoutInit env signal = 
      if(estInit env signal)
        then raise SignalDejaInit
        else (Init(signal))::env

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

 
    (* Dans le cas où le signal attendu n'est pas emit on applique le second choix*)
    let rec secondChoix st =
      match st with
        [] -> []
        | (signal,Save(s,e,((Present_SECD(signal1,c1,c2))::c),d))::t -> (Save( s , e , (append c2 c) , d ))::(secondChoix t)
        | _ -> raise EtatStuckInvalide

    (* Donne la liste des éléments de stuck qui réagisse au signal*)
    let rec reveil signal stuck =
      match stuck with
       [] -> []
       | (signal1,save)::t -> if (equal signal signal1) then save::(reveil signal t) else (reveil signal t)

    (* Donne la liste des éléments de stuck qui ne réagisse pas au signal *)
    let rec resteStuck signal stuck =
      match stuck with
        [] -> []
        | (signal1,save)::t -> 
          if (equal signal signal1) then (resteStuck signal t) else (signal1,save)::(resteStuck signal t)
    
    (* Ajoute un signal à la liste de signaux émits *)
    let rec ajoutSI si signal = if (estEmit si signal) then si else append si [(Emit_SI(signal))]




    (**** Machine SECD ****)

    (* Applique les règles de la machine SECD en affichant les étapes *)
    let rec machineSECD machine =
      
      afficherSECD machine ; 
      match machine with

        (* Si on a une constante en tête de la chaîne de contrôle, on la mets dans la pile *)
        MachineSECD(s,e,(Const_C b)::c,w,st,si,d,h) -> machineSECD (MachineSECD( (Fermeture_secd([Const_C b] , e) :: s) , e , c , w , st , si , d , h ))

        (* Si on a une variable en tête de la chaîne de contrôle, on la substitue par son élément lié dans l'environnement et on la mets dans la pile *)
        | MachineSECD(s,e,(Var_C x)::c,w,st,si,d,h) -> machineSECD (MachineSECD( ((substitution_secd x e) :: s) , e , c , w , st , si , d , h))

        | MachineSECD(s,e,(Throw_SECD erreur)::c,w,st,si,d,h) -> [Throw_SECD erreur] (* ce cas à modifier en vérifiant la présence d'un catch *)

        (* Si on a un opérateur, on prends le nombre d'opérande requis par l'opérateur et on fais le calcul dans la pile *)
        | MachineSECD(s,e,(Prim(op))::c,w,st,si,d,h) ->
            begin
              let nbrOperande = getNbrOperande op in 
              try machineSECD (MachineSECD (
                                (Fermeture_secd(secdLanguage_of_exprISWIM (calcul op ( rev (convert_liste_fermeture_secd_liste_int s nbrOperande))),[]))::(nbrElemRetirer s nbrOperande)
                                , e , c , w , st , si , d , h ))
              with FormatOpErreur -> machineSECD (MachineSECD([],[],[Throw_SECD(5)],[],[],[],Vide_D,Vide_H))
            end
        
        (* Si on a une abstraction dans la chaîne de contrôle, on la mets dans la pile *)
        | MachineSECD(s,e,(Pair(abs,control_string))::c,w,st,si,d,h) -> machineSECD (MachineSECD( (Fermeture_secd([Pair(abs,control_string)],e) :: s) , e , c , w , st , si , d , h ))
                                       
        (* Si on a un remplacement dans la pile et une application dans la chaîne de contrôle, on enlève le remplacement ainsi que l'application *)
        | MachineSECD(v::Remp::s,e,(Ap)::c,w,st,si,d,h) -> machineSECD (MachineSECD( v::s , e , c , w , st , si , d , h ))
        
        (* Si on a un remplacement dans la pile et une application dans la chaîne de contrôle, on enlève le remplacement ainsi que l'application *)
        | MachineSECD(Remp::v::s,e,(Ap)::c,w,st,si,d,h) -> machineSECD (MachineSECD( v::s , e , c , w , st , si , d , h ))

        (* Si on a une variable et une abstraction dans cette ordre dans la pile, on sauvegarde l'état de la machine on prend l'expression de l'abstraction 
        on la mets dans la chaîne de contrôle et on ajoute le lien entre la variable, la variable de l'abstraction et l'environnement de la fermeture de l'abstraction dans l'environnement *)
        | MachineSECD(v::( Fermeture_secd([Pair(abs,c1)],e1))::s,e,(Ap)::c,w,st,si,d,h) -> 
            begin
              try machineSECD (MachineSECD( [] , (ajoutEnv_secd e1 abs v) , c1 , w , st , si , Save(s,e,c,d) , h ))
              with EtatEnvInvalide -> machineSECD (MachineSECD([],[],[Throw_SECD 4],[],[],[],Vide_D,Vide_H))
            end

        (* Si la chaîne de contrôle est vide mais pas le dépôt, on prends la sauvegarde et on la replace dans la machine *)
        | MachineSECD(v::s,e,[], w , st , si ,Save(s1,e1,c,d),h) -> machineSECD (MachineSECD( v::s1 , e1 , c , w , st , si , d , h ))

        (* Si on a un Bspawn, on mets un remplacement dans la pile on prends la chaîne de caractère privée de la partie de la chaîne de contrôle étant entre Bspawn et Espawn 
        et on mets la partie retirée dans la liste d'attente dans une sauvegarde *)
        | MachineSECD(s,e,(Bspawn)::c,w,st,si,d,h) -> 
          begin
            try machineSECD (MachineSECD( Remp::s , e , (spawnRetirer c) , (append w [(Save(s,e,(spawnRecup c),d))] ), st , si , d , h ))
            with FinSpawnNonTrouvable -> machineSECD (MachineSECD([],[],[Throw_SECD(6)],[],[],[],Vide_D,Vide_H))
          end

        (* Si on a un signal, on l'ajoute dans l'environnement on fait une sauvegarde et on mets l'expression lié au signal dans la chaîne de contrôle*)
        | MachineSECD(s,e,(Signal_SECD(signal,c1))::c,w,st,si,d,h) -> 
          begin
            try machineSECD (MachineSECD( [] , (ajoutInit e signal) , c1 , w , st , si , Save(s,e,c,d) , h ))
            with SignalDejaInit -> machineSECD (MachineSECD([],[],[Throw_SECD (8)],[],[],[],Vide_D,Vide_H))
          end

        (* Si on a un present, plusieurs choix sont possibles *)
        | MachineSECD(s,e,(Present_SECD(signal,c1,c2))::c,w,st,si,d,h) ->
          if (estInit e signal)

            (* le signal attendu est initialisé *)
            then 
              if (estEmit si signal)

                (* le signal attendu est émit, du coup on prend la 1ère possibilité du present *)
                then machineSECD (MachineSECD( s , e ,(append c1 c) , w , st , si , d , h ))

                (* le signal attendu n'est pas émit, plusieurs choix s'offrent à nous *)
                else 

                  (* on regarde dans la liste d'attente *)
                  match w with

                    (* elle est vide, du coup on mets à vide tout excepté la liste de bloqué où l'on ajoute un élément qui est une sauvegarde de l'état de la machine avec le signal attendu *)
                    [] -> machineSECD (MachineSECD( [] , [] , [] , [] , (append st [(signal,(Save(s,e,(Present_SECD(signal,c1,c2))::c,d)))]) , si , Vide_D , h))

                    (* elle n'est pas vide, du coup on sauvegarde l'état avec le signal attendu dans la liste des bloqués et on remplace par la sauvegarde en tête de liste de la liste en attente *)
                    | (Save(s,e,c3,d))::t -> machineSECD (MachineSECD( s , e , c3 , t , (append st [(signal,(Save(s,e,(Present_SECD(signal,c1,c2))::c,d)))]) , si , Vide_D , h))

                    (* elle n'est pas de la forme attendu du coup on lève une exception *)
                    | _ -> machineSECD (MachineSECD([],[],[Throw_SECD(3)],[],[],[],Vide_D,Vide_H))
            
            (* le signal n'est pas initialisé donc on lève une exception *)
            else machineSECD (MachineSECD([],[],[Throw_SECD (7)],[],[],[],Vide_D,Vide_H))

        (* Si la chaîne de contrôle ainsi que le depôt sont vide on prend dans la liste d'attente la tête qui est une sauvegarde que l'on mets comme nouvelle état de la machine *)
        | MachineSECD(s1,e1,[],(Save(s,e,c,d))::w,st,si,Vide_D,h) -> machineSECD (MachineSECD( s , e , c , w , st , si , d , h ))

        (* Si on a un émit, on ajoute un remplacement dans la pile, on ajoute le signal émit dans la liste des signaux émits 
        et on réveil les élément qui attendent dans la liste bloqués et on les mets dans la liste d'attente *)
        | MachineSECD(s,e,(Emit_SECD(signal))::c,w,st,si,d,h) -> machineSECD (MachineSECD( Remp::s , e , c , (append (reveil signal st) w) , (resteStuck signal st) , (ajoutSI si signal) , d , h ))
        
        (* Si tout est vide excepté la liste bloqués et la liste des signaux émits, on mets tous les éléments bloqués dans la liste d'attente avec leurs secondes options choisies 
        et on vide la liste de signaux émits *)
        | MachineSECD([],[],[],[],st,si,Vide_D,h) -> 
          begin 
            try machineSECD (MachineSECD( [] , [] , [] , (secondChoix st) , [] , [] , Vide_D , h ))
            with EtatStuckInvalide -> machineSECD (MachineSECD([],[],[Throw_SECD 2],[],[],[],Vide_D,Vide_H))
          end

        (* Si tout est vide excepté la pile, l'environnement et la liste de signaux émit, on finit le focntionnement de la machine et on retourne l'élément en tête de pile. Ici une constante *)
        | MachineSECD(( Fermeture_secd([Const_C const],env))::s,e,[],[],[],si,Vide_D,h) -> [Const_C const]

        (* Si tout est vide excepté la pile, l'environnement et la liste de signaux émit, on finit le focntionnement de la machine et on retourne l'élément en tête de pile. Ici un remplacement *)
        | MachineSECD(Remp::s,e,[],w,st,si,Vide_D,h) -> [Var_C "unit"]

        (* Si tout est vide excepté la pile, l'environnement et la liste de signaux émit, on finit le focntionnement de la machine et on retourne l'élément en tête de pile. Ici une abstraction *)
        | MachineSECD(( Fermeture_secd([Pair(abs,c)],env))::s,e,[],[],[],si,Vide_D,h) -> [Pair(abs,c)]

        (* Cet état est inconnu donc on lève une erreur *)
        | _ -> machineSECD (MachineSECD([],[],[Throw_SECD 1],[],[],[],Vide_D,Vide_H))

    (* Lance et affiche le résultat de l'expression *)
    let lancerSECD expression =
       printf "Le résultat est %s \n" (string_of_control_string (machineSECD (MachineSECD([],[],(secdLanguage_of_exprISWIM expression),[],[],[],Vide_D,Vide_H))))
    
  end