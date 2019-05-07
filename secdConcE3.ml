open String ;;
open Printf ;;
open List ;;
open LangISWIMConc3E.ISWIM ;;


module SECDMachine =
  struct




    (**** Types ****)

    (* Petits types très pratique pour ne pas se mélanger dans la compréhension des types suivants *)
    type id_signal  =   string
    type variable   =   string
    type id_thread  =   int
    type id_error   =   int
    type emit       =   bool




    (**** Control string ****)

    (* type intermédiaire qui va servir à représenter la chaîne de contrôle *)
    type c =
        Constant of int                                         (* constante b               *)
      | Variable of variable                                    (* variable X                *)
      | Signal of id_signal
      | Error of id_error                                       (* une erreur traitée        *)
      | Ap                                                      (* application               *)
      | Prim of operateur                                       (* opérateur                 *)
      | Pair of variable * c list                               (* abstraction               *)
      | Bspawn                                                  (* début du spawn            *)
      | Espawn                                                  (* fin du spawn              *)
      | Emit                                                    (* emet                      *)
      | Present of c list * c list                              (* present s in t1 t2        *)
      | InitSignal                                              (* initialise un signal      *)
      | Throw                                                   (* lève l'erreur             *)
      | Catch of c list * (variable * c list)                   (* try and catch classique   *)
      | Put                                                     (* place dans un signal      *)
      | Get                                                     (* prends dans un signal     *)

    (* Ce type représente la chaîne de contrôle de la machine SECD, c'est notre entrée *)
    type control_string = c list   


    

    (**** Environment ****)

    (* type intermédiaire qui va servir à représenter l'environnement *)
    type e =   
        EnvClos of (variable * (control_string * e list))     (* (X,(C,Env))                   *)
      | EnvVar  of (variable * control_string)                (* (X,V) V une constante/erreur  *)

    (* Ce type représente l'environnement de la machine SECD, c'est notre liste de substitution *)
    type environment = e list





    (**** Stack ****)

    (* type intermédiaire contenant une fermeture qui lie une abstraction à un environnement ou juste une constante ou encore une erreur *)
    type s =  
        Stack_signal of id_signal 
      | Stack_const of int                                    (* constante            *)
      | Stack_error of id_error                               (* une erreur           *)
      | Stack_throw of id_error                               (* une erreur levé      *)
      | Closure of (control_string * environment)             (* fermeture (C,Env)    *)

    
    (* Ce type représente la pile de la machine SECD, c'est la où la machine travaille *)
    type stack = s list
    
    


    (**** Signals ****)

    (* type intermédiaire contenant pour un id de thread donné, sa liste de signaux qui ont eux même 
       leurs liste de valeurs, un booléen qui représente leurs initialisations et un autre leurs émissions *)
    type cs = CS of id_signal * (id_thread * int list) list * emit

    (* Ce type représente les signaux courants, c'est-à-dire, les signaux qui sont initialisé et émit à l'instant courant *)
    type current_signals = cs list

    (* type intermédiaire contenant pour un id de thread donné, sa liste de signaux qui ont eux mếme leurs liste de valeurs *)
    type ssi = SSI of id_signal * ((id_thread * ((int * (id_thread list)) list) * id_thread list ) list)

    (* Ce type représente les signaux partagés, c'est-à-dire, les signaux qui ont été émit à l'instant précédent *)
    type shared_signals = ssi list

    (* Les signaux sont regroupés dans un type qui est une pair comprenant 
       les signaux courants et les signaux partagés qui sont les signaux de l'instant précédent *)
    type signals = current_signals * shared_signals




    (**** Dump ****)

    (* Ce type représente le dépôt de la machine SECD, c'est-à-dire, l'endroit où l'on sauvegarde une partie de l'état 
       de la machine pour travailler sur une autre partie de la chaîne de contrôle *)
    type dump =
        Vide_D                                                                      (* le dépôt est vide      *)
      | Save       of stack * environment * control_string * dump                   (* (s,e,c,d)              *)




    (**** Thread list ****)

    (* type intermédiaire représentant un thread, comprenant son identifiant, sa pile , son environnement, sa chaîne de contrôle et son dépôt *)
    type thread = Thread of id_thread * stack * environment * control_string * dump

    (* Ce type représente la file d'attente de la machine SECD version concurrente, c'est-à-dire, 
       la file contenant les threads qui doivent être traité dans l'instant courant *)
    type wait = thread list

    (* Ce type représente la file de thread bloqués de la machine SECD version concurrente, c'est-à-dire, 
       la file contenant les threads qui sont en attente d'un signal ou juste de la fin de l'instant courant *)
    type stuck = (id_signal * thread) list

    (* Ce type représente l'ensemble des threads en cours de la machine SECD version concurrente. Cette ensemble est divisé en deux dans une pair, 
       d'un côté la liste d'attente de leurs tours et de l'autre la lite de threads bloqués *)
    type thread_list = wait * stuck


    

    (**** Identifier producer ****)

    (* Ce type représente un producteur d'identifiant de la machine SECD version concurrente, c'est-à-dire,
       un entier qui va donné un identifiant unique à chaque thread et s'incrémenter *)
    type identifier_producer = int




    (**** Handler ****)

    (* Ce type représente le gestionnaire de la machine SECD, c'est-à-dire, 
       l'endroit où l'on met une sauvegarde complète de la machine quand on a un try and catch *)
    type handler = 
        Vide_H                                                (* le handler est vide        *)
      | SaveHandler of id_error            *                  (* (erreur,(id,s,e,c,d,h,ip)) *)
                                              (
                       id_thread           * 
                       stack               * 
                       environment         * 
                       control_string      *
                       thread_list         * 
                       signals             * 
                       dump                * 
                       handler             *
                       identifier_producer    ) 


    

    (**** Machine SECD version concurrente ****)

    (* Ce type représente la structure de la machine SECD version concurrente *)
    type secd = MachineSECD of id_thread           * 
                               stack               * 
                               environment         * 
                               control_string      * 
                               thread_list         * 
                               signals             * 
                               dump                * 
                               handler             *
                               identifier_producer 










    (**** Exception ****)

    exception NoSubPossible                    (* Aucune substitution possible pour cette variable dans l'environnement             *)
    exception StrangeEnd                       (* Même moi je ne sais pas ce qu'il sait passé ...                                   *)
    exception EndSpawnNotFound                 (* La délimitation de fin du spawn n'est pas trouvé dans la chaîne de contrôle       *)
    exception NoSharedValues                   (* Il n'y a pas de valeurs partagées                                                 *)

    exception NotAllConstants                  (* Tous les éléments de la pile prisent pour l'opérateurs ne sont pas des constantes *)
    exception InsufficientOperandNb            (* Le nombre d'opérande est insuffisante par rapport au nombre requis                *)
    exception ImpossibleResult                 (* Le résultat n'est pas dans un format autorisé, soit un entier ou une abstraction  *)
    exception DivZero

    exception SignalAlreadyInit                (* Le signal est déjà initialisé dans ce thread                                      *)
    exception SignalAlreadyEmit                (* Le signal est déjà émit dans la machine                                           *)
    exception SignalNotInit                    (* Le signal n'est pas initialisé dans ce thread                                     *)
    exception SignalNotShared                  (* Le signal n'est pas dans la liste des signaux partagés                            *)
    exception ThreadSharedNotFound             (* L'identifiant de thread lié à un signal n'existe pas                              *)
    exception PointerNotExist                  (* Le pointeur n'existe pas                                                          *)
    exception EndIt                            (* L'itérateur a finit son parcours                                                  *)

    exception UnknowState                      (* Le format de la machine est invalide et/ou inconnu                                *)
    exception UnknowStackState                 (* Le format de la pile est invalide et/ou inconnu                                   *)
    exception UnknowEnvState                   (* Le format de l'environnement est invalide et/ou inconnu                           *)
    exception UnknowWaitState                  (* Le format de la liste d'attente est invalide et/ou inconnu                        *)
    exception UnknowStuckState                 (* Le format de la liste d'élément bloqués est invalide et/ou inconnu                *)
    exception UnknowDumpState                  (* Le format du dépôt est invalide et/ou inconnu                                     *)
    exception UnknowHandlerState               (* Le format du gestionnaire d'erreur est invalide et/ou inconnu                     *)
    exception UnknowSSIState                   (* Le format de la liste de signaux partagés est invalide                            *)










    (**** Affichage ****)

    (* Concatène une liste de chaîne de caractères en une seule chaîne de caractères *)
    let rec concat_secd_list secd_list =
      match secd_list with
          []    ->   "" 

        | [h]     ->   h
        
        | h::t  ->   h^";"^(concat_secd_list t)


    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
          Const const                           ->   [Constant const]
            
        | Var var                               ->   [Variable var]
            
        | App(expr1,expr2)                      ->   append (  append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Op(op,liste_expr)                     ->   append ( flatten( map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Abs(abs,expr)                         ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]

        | Spawn expr                            ->   append [Bspawn] (append (secdLanguage_of_exprISWIM expr) [Espawn])

        | Present_ISWIM (signal,expr1,expr2)    ->   [Signal signal ; Present ((secdLanguage_of_exprISWIM expr1),(secdLanguage_of_exprISWIM expr2))]

        | Emit_ISWIM (signal)                   ->   [Signal signal ; Emit]

        | Signal_ISWIM signal                   ->   [Signal signal ; InitSignal]

        | Throw_ISWIM error                     ->   [Error error ; Throw]

        | Catch_ISWIM(error,expr1,(abs,expr2))  ->   [Error error ; Catch((secdLanguage_of_exprISWIM expr1),(abs,(secdLanguage_of_exprISWIM expr2)))]

        | Put_ISWIM(signal,value)               ->   [Constant value ; Signal signal ; Put]

        | Get_ISWIM(signal,id_thread)           ->   [Constant id_thread ; Signal signal ; Get]


    (* Donne une chaîne de caractères contenant un message d'erreur par rapport à l'identifiant de l'erreur *)
    let error_message error =
      match error with
          0  ->  "ERREUR : Etrange..."                                           (* StrangeEnd            *)

        | 1   ->   "ERREUR : Format de la machine invalide :/"                   (* UnknowState           *)
        | 2   ->   "ERREUR : Format de la liste bloqué invalide"                 (* UnknowStuckState      *)
        | 3   ->   "ERREUR : Format de la liste d'attente invalide"              (* UnknowWaitState       *) 
        | 4   ->   "ERREUR : Format de l'environnement invalide"                 (* UnknowEnvState        *)
        | 5   ->   "ERREUR : Format de la pile invalide"                         (* UnknowStackState      *)
        | 6   ->   "ERREUR : Format du dépôt invalide"                           (* UnknowDumpState       *)
        | 7   ->   "ERREUR : Format du gestionnaire invalide"                    (* UnknowHandlerState    *)

        | 8   ->   "ERREUR : Signal non initialisé"                              (* SignalNotInit         *)
        | 9   ->   "ERREUR : Signal déjà initialisé"                             (* SignalAlreadyInit     *)
        
        | 10  ->   "ERREUR : Aucune substitution possible"                       (* NoSubPossible         *)

        | 11  ->   "ERREUR : Le format du spawn est invalide"                    (* EndSpawnNotFound      *)

        | 12  ->   "ERREUR : Ce ne sont pas tous des constantes"                 (* NotAllConstants       *)
        | 13  ->   "ERREUR : Division par zero"                                  (* DivZero               *)
        | 14  ->   "ERREUR : Nombre d'opérande insuffisant"                      (* InsufficientOperandNb *)
        | 15  ->   "ERREUR : Format de l'opérateur invalid"                      (* OpFormatError         *)
        | 16  ->   "ERREUR : Résultat impossible"                                (* ImpossibleResult      *)

        | 17  ->   "ERREUR : Aucune valeurs partagées"                           (* NoSharedValues        *)
        | 18  ->   "ERREUR : Le signal n'a pas été partagé"                      (* SignalNotShared       *)
        | 20  ->   "ERREUR : Le thread n'est pas trouvé"                         (* ThreadSharedNotFound  *)
        | 21  ->   "ERREUR : Le pointeur n'existe pas"                           (* PointerNotExist       *)
        | 22  ->   "Fin d'un itérateur"                                          (* EndIt                 *)
 
        | _   ->   "ERREUR : Cette erreur n'existe pas "


    (* Convertit la chaîne de contrôle en une chaîne de caractères *)
    let rec string_of_control_string expression =
      match expression with

          []                                 ->   ""
      
        | Constant const::t                  ->   (string_of_int const)^" "^(string_of_control_string t)
      
        | Variable var::t                    ->   var^" "^(string_of_control_string t)
      
        | Ap::t                              ->   "ap "^(string_of_control_string t)

        | Signal signal::t                   ->   signal^" "^(string_of_control_string t)
      
        | Pair(abs,expr_list)::t             ->   "("^abs^",("^(string_of_control_string expr_list)^")) "^(string_of_control_string t)
      
        | Prim(op)::t                        ->   "prim "^(string_of_operateur op)^" "^(string_of_control_string t)

        | Bspawn::t                          ->   "bspawn "^(string_of_control_string t)

        | Espawn::t                          ->   "espawn "^(string_of_control_string t)

        | Present(expr1,expr2)::t            ->    "present in "^(string_of_control_string expr1)
                                                  ^(string_of_control_string expr2)^(string_of_control_string t)

        | Emit::t                            ->   "emit "^(string_of_control_string t)

        | InitSignal::t                      ->   "init "^(string_of_control_string t)

        | Throw::t                           ->   "throw "^(string_of_control_string t)

        | Catch(expr1,(abs,expr2))::t        ->    "try "^(string_of_control_string expr1)
                                                  ^"catch ("^abs^" , "^(string_of_control_string expr2)^")"^(string_of_control_string t) 

        | Error error::t                     ->   (error_message error)^" "^(string_of_control_string t)

        | Put::t                             ->   "put "^(string_of_control_string t)
       
        | Get::t                             ->   "get "^(string_of_control_string t)


    (* Convertit un environnement en chaîne de caractères *)
    let rec string_of_environment environment =
      match environment with
          []                                      ->   ""

        | (EnvClos(var,(control_string,env)))::t  ->   "["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_environment env)^"]] , "^(string_of_environment t)

        | (EnvVar(var,control_string))::t         ->   "["^var^" , "^(string_of_control_string control_string) ^"] , "^(string_of_environment t)


    (* Convertit une pile en chaîne de caractères *)
    let rec string_of_stack stack =
      match stack with
          []                                ->   ""

        | Stack_signal signal::t            ->   signal^" "^(string_of_stack t)

        | Stack_const b::t                  ->   (string_of_int b)^" "^(string_of_stack t)

        | Stack_error e::t                  ->   "Erreur "^(string_of_int e)^" "^(string_of_stack t)

        | Stack_throw e::t                  ->   "Erreur levé "^(string_of_int e)^" "^(string_of_stack t)

        | (Closure(control_string,env))::t  ->   "["^(string_of_control_string control_string)^" , {"^(string_of_environment env)^"}]"^(string_of_stack t)
        

    (* Convertit la sauvegarde en chaîne de caractères *)
    let rec string_of_dump dump =
      match dump with 
          Vide_D                                            ->   ""

        | Save(stack,env,control_string,dump)               ->    "("^(string_of_stack stack)^" , "^(string_of_environment env)^" , "
                                                                 ^(string_of_control_string control_string)^" , "^(string_of_dump dump)^")"
        

      
    (* Convertit un thread en chaîne de caractères *)
    let rec string_of_thread thread =
      match thread with
        Thread(id,stack,env,control_string,dump)  ->    "("^(string_of_int id)^" , "^(string_of_stack stack)^" , "
                                                       ^(string_of_environment env)^" , "^(string_of_control_string control_string)^" , "
                                                       ^(string_of_dump dump)^")"


    (* Convertit la liste des éléments en attente en chaîne de caractères *)
    let rec string_of_wait wait =
      match wait with 
          []         ->   "" 
        
        | [thread]   ->   (string_of_thread thread)
        
        | thread::t  ->   (string_of_thread thread)^" , "^(string_of_wait t)


    (* Convertit la liste des éléments bloqués en chaîne de caractères *)
    let rec string_of_stuck stuck =
      match stuck with 
          []                  ->   "" 

        | [(signal,thread)]   ->   "( "^signal^", "^(string_of_thread thread)^" )"

        | (signal,thread)::t  ->   "( "^signal^", "^(string_of_thread thread)^" ) , "^(string_of_stuck t)


    (* Convertit la liste des threads en chaîne de caractères *)
    let rec string_of_thread_list thread_list =
      match thread_list with
        (wait,stuck)  ->    "\n     WAIT    : "^(string_of_wait wait)
                           ^"\n     STUCK   : "^(string_of_stuck stuck)

    
    

    (* Convertit une liste de signaux courant en chaîne de caractères *)
    let rec string_of_cs cs_list =
      match cs_list with
          []                               ->   ""

        | (id_thread,values)::t       ->    "("^(string_of_int id_thread)^",{"^(concat_secd_list(map string_of_int values))^"}) "^(string_of_cs t)


    (* Convertit la liste des signaux courant liés à leurs threads en chaîne de caractères *)
    let rec string_of_current_signals current_signals =
      match current_signals with
          []                                 ->   "" 

        | [CS(id_signal,thread_list,emit)]   ->   "["^id_signal^" : {"^(string_of_cs thread_list)^","^(string_of_bool emit)^"}]"

        | CS(id_signal,thread_list,emit)::t  ->   "["^id_signal^" : {"^(string_of_cs thread_list)^","^(string_of_bool emit)^"}] ; "^(string_of_current_signals t) 


    (* Convertit la liste des signaux partagés en chaîne de caractères *)
    let rec string_of_ssi ssi_list = 
      let rec aux values =
        match values with
            []                                 ->   ""

          | [(value,pointers)]                 ->   "("^( string_of_int value)^",{"^(concat_secd_list(map string_of_int pointers))^"})"

          | (value,pointers)::t                ->   "("^( string_of_int value)^",{"^(concat_secd_list(map string_of_int pointers))^"});"^(aux t)
      in
      match ssi_list with
          []                                   ->   ""

        | (id_thread,values,thread_list)::t    ->   " ("^(string_of_int id_thread)^",["^(aux values)^"],{"^(concat_secd_list( map string_of_int thread_list))^"}) "^(string_of_ssi t)

    
    (* Convertit la liste des signaux partagés lié à leurs threads en chaîne de caractères *)
    let rec string_of_shared_signals shared_signals =
      match shared_signals with
          []                             ->   ""

        | [SSI(id_signal,thread_list)]   ->   "["^id_signal^" : {"^(string_of_ssi thread_list)^"}]" 

        | SSI(id_signal,thread_list)::t  ->   "["^id_signal^" : {"^(string_of_ssi thread_list)^"}] , "^(string_of_shared_signals t)  


    (* Convertit la liste de tous les signaux en chaîne de caractères *)
    let rec string_of_signals signals =
      match signals with
        (current_signals,shared_signals)  ->    "\n     CURRENT  : "^(string_of_current_signals current_signals)
                                               ^"\n     SHARED   : "^(string_of_shared_signals shared_signals)

    
    (* Convertit le gestionnaire en chaîne de caractères *)
    let rec string_of_handler handler =
      match handler with
          Vide_H                                                                                                 ->   ""
        
        | SaveHandler(error,(id,stack,env,control_string,thread_list,signals,dump,handler,identifier_producer))  ->    
                                        "\n      ERREUR  : "^(error_message error)
                                       ^"\n      ID      : "^(string_of_int id)
                                       ^"\n      STACK   : "^(string_of_stack stack)
                                       ^"\n      ENV     : "^(string_of_environment env)
                                       ^"\n      CONTROL : "^(string_of_control_string control_string)
                                       ^"\n      THREADS : "^(string_of_thread_list thread_list)
                                       ^"\n      SIGNALS : "^(string_of_signals signals)
                                       ^"\n      DUMP    : "^(string_of_dump dump)
                                       ^"\n      HANDLER : "^(string_of_handler handler)
                                       ^"\n      IP      : "^(string_of_int identifier_producer)
                                       ^"\n"


    (* Convertit une machine SECD en chaîne de caractères *)
    let rec string_of_secdMachine machine =
      match machine with
        MachineSECD(id,stack,env,control_string,thread_list,signals,dump,handler,identifier_producer)  ->    
                                        "\n    ID      : "^(string_of_int id)
                                       ^"\n   STACK   : "^(string_of_stack stack)
                                       ^"\n   ENV     : "^(string_of_environment env)
                                       ^"\n   CONTROL : "^(string_of_control_string control_string)
                                       ^"\n   THREADS : "^(string_of_thread_list thread_list)
                                       ^"\n   SIGNALS : "^(string_of_signals signals)
                                       ^"\n   DUMP    : "^(string_of_dump dump)
                                       ^"\n   HANDLER : "^(string_of_handler handler)
                                       ^"\n   IP      : "^(string_of_int identifier_producer)
                                       ^"\n\n"


    (* Affiche la machine SECD *)
    let afficherSECD machine = printf "MachineSECD : %s\n" (string_of_secdMachine machine)










    (**** Fonctions utiles ****)

    (* Substitue une variable à sa  fermeture liée *)
    let rec substitution x env =
      match env with
          []                                    ->   raise NoSubPossible

        | EnvClos(var,(control_string,env))::t  ->   if (equal x var) then  Closure(control_string,env) else substitution x t

        | EnvVar(var,control_string)::t         ->   if (equal x var) 
                                                      then 
                                                        begin
                                                          match control_string with
                                                              [Constant b]  ->   Stack_const b 
                                                            
                                                            | [Error e]     ->   Stack_error e

                                                            | _             ->   raise UnknowEnvState
                                                        end
                                                      else substitution x t
        


    (* Convertit une liste d'élément de la pile contenant des constantes en liste d'entier *)
    let rec convert_stack_element_to_int_list stack nbrOperande =
      match (stack,nbrOperande) with
          (_,0)                       ->   []

        | (Stack_const b::t,nbr)      ->   b::(convert_stack_element_to_int_list t (nbr - 1)) 

        | (Stack_error e::t,nbr)      ->   e::(convert_stack_element_to_int_list t (nbr - 1))

        | (Stack_throw e::t,nbr)      ->   e::(convert_stack_element_to_int_list t (nbr - 1))

        | (_,nbr)                     ->   if nbr = 0 then raise NotAllConstants else raise OpFormatError


    (* Retire un nombre n d'élément de la pile *)
    let rec remove_elements stack nbrOperand =
      match (stack,nbrOperand) with
          (stack,0)   ->   stack

        | (h::t,nbr)  ->   if nbr > 0 then remove_elements t (nbr - 1) else raise InsufficientOperandNb

        | ([],_)      ->   raise InsufficientOperandNb


    (* Vérifie si une variable est dans l'environnement *)
    let rec in_environment env var =
      match env with
          []                                     ->   false

        | EnvClos(var1,(control_string,env))::t  ->   if (equal var1 var) then true else in_environment t var 

        | EnvVar(var1,control_string)::t         ->   if (equal var1 var) then true else in_environment t var 


    (* Vérifie si c'est un init *)
    let rec isInit current_signals id signal =
      match current_signals with
          []                  ->   false

        | CS(signal1,_,_)::t  ->   if (signal = signal1) then true else (isInit t id signal)



    (* Vérifie si c'est une émission *)
    let rec isEmit current_signals signal =
      match current_signals with
          []                     ->   false

        | CS(signal1,_,emit)::t  ->   if (signal = signal1) then emit else (isEmit t signal)



    (* Ajoute une fermeture à l'environnement *)
    let add_environment env varToRep stack_element =
      if(in_environment env varToRep) 
        then env 
        else  match stack_element with

                  Stack_const b                 ->   (EnvVar(varToRep,[Constant b]))::env

                | Stack_error error             ->   (EnvVar(varToRep,[Error error]))::env

                | Closure(control_string,env1)  ->   (EnvClos(varToRep,(control_string,env1)))::env

                | _                             ->   raise UnknowEnvState


    (* Ajoute un signal initialisé dans l'environnement *)
    let rec add_init_signal current_signals id signal = 
      let rec aux1 threads =
        match threads with
            (id1,values)::t                  ->   if (id = id1) then raise SignalAlreadyInit else append [(id1,values)] (aux1 t)

          | []                                    ->   [(id,[])]
      in
      match current_signals with
          CS(signal1,threads,emit)::t           ->   if (signal = signal1) then (append [CS(signal,(aux1 threads),emit)] t) else (append [CS(signal1,threads,emit)] (add_init_signal t id signal))
        
        | []                                    ->   [CS(signal,[(id,[])],false)]
        


    (* Retire la partie du spawn de la chaîne de contrôle *)
    let rec remove_spawn control_string =
      match control_string with
          []         ->   raise EndSpawnNotFound

        | Espawn::t  ->   t

        | h::t       ->   remove_spawn t


    (* Récupère la partie du spawn de la chaîne de contrôle *)
    let rec recover_spawn control_string =
      match control_string with
          []         ->   raise EndSpawnNotFound
        
        | Espawn::t  ->   []
        
        | h::t       ->   h::(recover_spawn t)

 
    (* Dans le cas où le signal attendu n'est pas emit on applique le second choix*)
    let end_of_the_moment_thread thread_list =
      let rec aux stuck =
        match stuck with
            []                                                          ->   []

          | (signal,Thread(id,Stack_signal si::s,e,((Present(c1,c2))::c),d))::t  ->   (Thread( id , s , e , (append c2 c) , d ))::(aux t)

          | _                                                           ->   raise UnknowStuckState
      in
      match thread_list with
        (wait,stuck)                                                    ->   ((aux stuck),[]) 


    (* Remet à zero les signaux en vidant la liste des valeurs et en mettant l'émission à faux *)
    let rec reset_current_signals current_signals =
      let rec aux thread_list =
        match thread_list with
            []                            ->   []

          | (thread,_)::t            ->   (thread,[])::(aux t)
      in
      match current_signals with
          []                              ->   []

        | CS(signal,thread_list,emit)::t  ->   CS(signal,(aux thread_list),false)::(reset_current_signals t)


    (* Prends les signaux dans la liste des signaux courants et les mets dans la liste des signaux partagés *)
    let rec share current_signals =
      let rec aux thread_list =
        match thread_list with
            []                             ->   []

          | (thread,values)::t           ->   (thread,(map (fun x -> (x,[])) values),[])::(aux t)
      in
      match current_signals with
          []                               ->   []

        | CS(signal,thread_list,true)::t   ->   SSI(signal,(aux thread_list))::(share t)

        | CS(signal,thread_list,false)::t  ->  (share t)


    (* Quand l'instant est fini ont remet à zéro les signaux courant et on change les signaux partagés *)
    let end_of_the_moment_signals signals =
      match signals with
        (current_signals,_)  ->   ((reset_current_signals current_signals),(share current_signals)) 


    (* Donne la liste des éléments de stuck qui réagisse au signal*)
    let rec wake_up signal stuck =
      match stuck with
          []                   ->   []

        | (signal1,thread)::t  ->   if (equal signal signal1) then thread::(wake_up signal t) else (wake_up signal t)


    (* Donne la liste des éléments de stuck qui ne réagisse pas au signal *)
    let rec remains_blocked signal stuck =
      match stuck with
          []                   ->   []

        | (signal1,thread)::t  ->   if (equal signal signal1) then (remains_blocked signal t) else (signal1,thread)::(remains_blocked signal t)


    (* Vérifie si l'émission d'un signal réveil des threads bloqués*)
    let check_thread_list signal thread_list = 
      match thread_list with
        (wait,stuck)  ->   ((append wait (wake_up signal stuck)),(remains_blocked signal stuck))
    

    (* Mets vrai pour l'émission du signal dans liste de signaux *)
    let rec emit_signal current_signals id signal =
      if isInit current_signals id signal
        then match current_signals with

                CS(signal1,threads,emit)::t  ->   if (signal = signal1) then (append [CS(signal1,threads,true)]  t) else (append [CS(signal1,threads,emit)]  (emit_signal t id signal))
                
              | []                           ->   raise SignalNotInit  

        else raise SignalNotInit

    
    (* Ajoute un élément dans la liste des variables liés au signal et à l'id du thread courant *)
    let rec put current_signals signal id b = 
      let rec aux1 signals =
        match signals with
          (id1,values)::t                ->   if (id = id1) then (append [(id1,(append values [b]))] t) else (append [(id1,values)]  (aux1 t))

        | []                             ->   raise SignalNotInit
      in
      match current_signals with
            CS(signal1,threads,emit)::t  ->   if (signal = signal1) then (append [CS(signal1,(aux1 threads),emit)]  t) else (append [CS(signal1,threads,emit)] (put t signal id b))

          | []                           ->   raise SignalNotInit


    (* Vérifie si c'est la première fois que l'on pioche dans une liste de valeurs *)
    let rec first_get id_thread values =
      match values with
          []                     ->   true

        | (_,pointers)::t        ->   if (mem id_thread pointers) then false else first_get id_thread t

    
    (* Ajoute un pointeur dans une liste de pointeurs *)
    let addIt it pointers = append pointers [it]


    (* Retire un pointeur d'une liste de pointeurs *)
    let removeIt it pointers =
      let rec aux pointers =
        match pointers with
            []    ->   []

          | h::t  ->   if (h = it) then aux t else h::(aux t)
      in
      if (mem it pointers) then aux pointers else raise PointerNotExist


    (* Retourne la valeur présent dans la liste de valeurs partagés lié au signal et à l'identifant donné 
       qui est la suivante de celle pointé part le pointeurs lié à ton thread *)    
    let rec get shared_signals id signal my_thread =
      let rec aux3 values =
        match values with
            []                       ->   raise UnknowSSIState

          | (value,pointers)::t      ->   (append [(value,(addIt my_thread pointers))] t)
      in
      let rec aux2 values =
        match values with
            []                       ->   raise UnknowSSIState

          | [(value,pointers)]       ->   if (mem my_thread pointers) 
                                            then (Stack_const value,[(value,(removeIt my_thread pointers))],true) 
                                            else raise UnknowSSIState

          | (value,pointers)::t      ->   if (mem my_thread pointers) 
                                            then let new_values               =   aux3 t in (Stack_const value,(append [(value,(removeIt my_thread pointers))] new_values),false) 
                                            else let (res,new_values,end_it)  =   aux2 t in (res,(append [(value,pointers)] new_values),end_it)
      in
      let rec aux1 threads =
        match threads with 
          
            (id1,values,end_list)::t  ->   if (id1 = id) 
                                            then if(mem my_thread end_list)
                                                    then raise EndIt
                                                    else if(first_get my_thread values) 
                                                            then  match values with 
                                                                      (value,pointers)::t1  ->   (Stack_const value,append [(id,append [(value,pointers)] (aux3 t1),end_list)] t) 
                                                                    | []                    ->   raise EndIt
                                                            else let (res,new_values,end_it)   =   aux2 values in 
                                                                                                        if end_it 
                                                                                                          then (res,append [(id,new_values,append [my_thread] end_list)] t)
                                                                                                          else (res,append [(id,new_values,end_list)] t) 

                                            else   let (res,new_threads)  =   aux1 t      in (res,append [(id,values,end_list)] new_threads)

          | [] -> raise ThreadSharedNotFound
      in
      match shared_signals with
          []                       ->   raise SignalNotShared

        | SSI(signal1,threads)::t  ->   if (signal1 = signal) 
                                            then let (res,new_threads)  =   aux1 threads                    in (res,(append [SSI(signal1,new_threads)]       t))  
                                            else let (res,new_ssi)      =   get t id signal my_thread       in (res,(append [SSI(signal1,threads)    ] new_ssi)) 





        
    (**** Machine SECD ****)

    (* Applique les règles de la machine SECD en affichant les étapes *)
    let rec machineSECD machine =
      (afficherSECD machine) ;
      match machine with
          (* On a une constante dans la chaîne de contrôle, on la place dans la pile *)
        | MachineSECD(id,s,e,Constant b::c,tl,si,d,h,ip)                          ->    machineSECD (MachineSECD( id , Stack_const b::s , e , c , tl , si , d , h , ip )) 


          (* On a une variable dans la chaîne de contrôle, on place sa substitution (stockée dans l'environnement) dans la pile *)
        | MachineSECD(id,s,e,Variable x::c,tl,si,d,h,ip)                          
          ->    begin 
                  try   machineSECD (MachineSECD( id , (substitution x e)::s , e , c , tl , si , d , h , ip )) 
                  with  NoSubPossible  -> machineSECD (MachineSECD( id , Stack_throw 10::s , e , c , tl , si , d , h , ip ))
                end


          (* On a prim dans la chaîne de contrôle, on prends le nombre d'élément nécessaire au bon fonctionnement de l'opérateur lié à prim dans la pile 
             et on effectue le calcul. On mets le résultat dans la pile *)
        | MachineSECD(id,s,e,Prim op::c,tl,si,d,h,ip)                             
          ->    begin 
                  match (s,op) with 
                      (Stack_const b::s,Add1)                  ->   machineSECD (MachineSECD( id , Stack_const(b+1)::s , e , c , tl , si , d , h , ip ))

                    | (Stack_const b::s,Sub1)                  ->   machineSECD (MachineSECD( id , Stack_const(b-1)::s , e , c , tl , si , d , h , ip ))

                    | (Stack_const 0::s,IsZero)                ->   machineSECD (MachineSECD( id , Closure([Pair("x",[Pair("y",[Variable "x"])])],e)::s , e , c , tl , si , d , h , ip ))

                    | (Stack_const b::s,IsZero)                ->   machineSECD (MachineSECD( id , Closure([Pair("x",[Pair("y",[Variable "y"])])],e)::s , e , c , tl , si , d , h , ip ))

                    | (Stack_const b::Stack_const b1::s,Add)   ->   machineSECD (MachineSECD( id , Stack_const(b1+b)::s , e , c , tl , si , d , h , ip ))  

                    | (Stack_const b::Stack_const b1::s,Sub)   ->   machineSECD (MachineSECD( id , Stack_const(b1-b)::s , e , c , tl , si , d , h , ip ))
            
                    | (Stack_const b::Stack_const b1::s,Mult)  ->   machineSECD (MachineSECD( id , Stack_const(b1*b)::s , e , c , tl , si , d , h , ip ))
            
                    | (Stack_const 0::Stack_const b1::s,Div)   ->   machineSECD (MachineSECD( id , Stack_throw 13::s , e , c , tl , si , d , h , ip ))

                    | (Stack_const b::Stack_const b1::s,Div)   ->   machineSECD (MachineSECD( id , Stack_const(b1/b)::s , e , c , tl , si , d , h , ip ))

                    | (Stack_throw 22::s,HasNext)              ->   machineSECD (MachineSECD( id , Stack_const 0::s , e , c , tl , si , d , h , ip ))  
                    
                    | (Stack_const b::s,_)                     ->   machineSECD (MachineSECD( id , Stack_throw 14::s , e , c , tl , si , d , h , ip ))

                    | (_,_)                                    ->   machineSECD (MachineSECD( id , Stack_throw 12::s , e , c , tl , si , d , h , ip )) 
                end


          (* On a une abstraction dans la chaîne de contrôle, on place une fermeture ,qui comporte l'abstraction et l'environnment courant, dans la pile *)
        | MachineSECD(id,s,e,Pair(abs,c1)::c,tl,si,d,h,ip)                        ->    machineSECD (MachineSECD( id, Closure([Pair(abs,c1)],e)::s , e , c , tl , si , d , h , ip ))  
        

          (* On a Ap dans la chaîne de contrôle, on sauvegarde une partie de la machine dans le dépôt, on prends l'environnement de la fermeture et on ajoute la nouvelle substitution *)
        | MachineSECD(id,v::Closure([Pair(abs,c1)],e1)::s,e,Ap::c,tl,si,d,h,ip)   ->    machineSECD (MachineSECD( id , [] , (add_environment e1 abs v) , c1 , tl , si , Save(s,e,c,d) , h , ip ))


          (* On a la chaîne de contrôle vide et le dépôt à une sauvegarde, on prends la sauvegarde et on l'applique sur la machine *)
        | MachineSECD(id,v::s,e,[],tl,si,Save(s1,e1,c,d),h,ip)                    ->    machineSECD (MachineSECD( id , v::s1 , e1 , c , tl , si , d , h , ip ))


          (* On a une variable dans la chaîne de contrôle, on place sa substitution (stockée dans l'environnement) dans la pile *)
        | MachineSECD(id,s,e,Error error::c,tl,si,d,h,ip)                         ->    machineSECD (MachineSECD( id , Stack_error error::s , e , c , tl , si , d , h , ip )) 


          (* On a une erreur levé dans la pile donc qu'importe ce que l'on a dans la chaîne de contrôle on propage (car la seule possibilité de traitement a déjà été fait) *)
        | MachineSECD(id,Stack_throw error::s,e,_::c,tl,si,d,h,ip)                ->    machineSECD (MachineSECD( id , Stack_throw error::s , e , c , tl , si , d , h , ip )) 


        | MachineSECD(id,Stack_throw er::s,e,[],tl,si,d,h,ip)                  
          ->    begin
                  match h with
                      Vide_H  ->    [Error er]

                    | SaveHandler(er1,(id1,s1,e1,Pair(abs,c1)::c,tl1,si1,d1,h1,ip1))  
                      ->    if (er == er1) 
                              then machineSECD (MachineSECD( id1 , [] , (add_environment e1 abs (Stack_error er)) , c1 , tl1 , si1 , Save(s1,e1,c,d1) , h1 , ip1 )) 
                              else machineSECD (MachineSECD( id , Stack_throw er::s , e , [] , tl , si , d , h1 , ip ))

                    | _       ->    raise UnknowHandlerState 
                end


          (* On a un gestionnaire d'erreur dans la chaîne de contrôle, on sauvegarde la machine entière dans le handler avec la seconde chaîne de contrôle avec l'erreur liée et  
             on mets la première chaîne de contrôle dans l'état courant*)
        | MachineSECD(id,Stack_error error::s,e,Catch(c1,(abs,c2))::c,tl,si,d,h,ip)            
          ->    machineSECD (MachineSECD( id , s , e , (append c1 c) , tl , si , d , SaveHandler(error,(id,s,e,(append [Pair(abs,c2)] c),tl,si,d,h,ip)) , ip ))



                                    
        

          (* On a un signal dans la chaîne de contrôle, on le mets dans la pile *)
        | MachineSECD(id,s,e,Signal signal::c,tl,si,d,h,ip)                       ->    machineSECD (MachineSECD( id , Stack_signal signal::s , e , c , tl , si , d , h , ip )) 


          (* On a Bspawn dans la chaîne de contrôle, on prends la partie de la chaîne de contrôle compris entre Bspawn et Espawn et le mets dans un nouveau thread *)
        | MachineSECD(id,s,e,Bspawn::c,(w,st),si,d,h,ip)                          
          ->    begin 
                  try   machineSECD (MachineSECD( id , s , e , (remove_spawn c) , (append w [Thread(ip,s,e,(recover_spawn c),d)],st) , si , d , h , (ip+1) ))
                  with  EndSpawnNotFound  ->   machineSECD (MachineSECD( id , Stack_throw 11::s , e , c , (w,st) , si , d , h , ip ))
                end

          (* On a put dans la chaîne de contrôle, on prend la constante en tête dans la pile et on la mets dans le signal *)
        | MachineSECD(id,Stack_signal signal::Stack_const b::s,e,Put::c,tl,(cs,ssi),d,h,ip)           
          ->    begin
                  try   machineSECD (MachineSECD( id , s , e , c , tl , ((put cs signal id b),ssi) , d , h , ip ))
                  with  SignalNotInit  ->   machineSECD (MachineSECD( id , Stack_throw 8::s , e , c , tl , (cs,ssi) , d , h , ip )) 
                end


          (* On a get dans la chaîne de contrôle, on prends la constante dans la liste des valeurs partagées liées à un signal et u identifant de thread *)
        | MachineSECD(id,Stack_signal signal::Stack_const b::s,e,Get::c,tl,(cs,ssi),d,h,ip)          
          ->    begin
                  try   let (res,new_ssi) = get ssi b signal id in 
                          machineSECD (MachineSECD( id , res::s , e , c , tl , (cs,new_ssi) , d , h , ip ))
                          
                  with    NoSharedValues        ->   machineSECD (MachineSECD( id , Stack_throw 17::s , e , c , tl , (cs,ssi) , d , h , ip )) 

                        | SignalNotShared       ->   machineSECD (MachineSECD( id , Stack_throw 18::s , e , c , tl , (cs,ssi) , d , h , ip )) 

                        | ThreadSharedNotFound  ->   machineSECD (MachineSECD( id , Stack_throw 19::s , e , c , tl , (cs,ssi) , d , h , ip ))

                        | PointerNotExist       ->   machineSECD (MachineSECD( id , Stack_throw 21::s , e , c , tl , (cs,ssi) , d , h , ip ))

                        | EndIt                 ->   machineSECD (MachineSECD( id , Stack_throw 22::s , e , c , tl , (cs,ssi) , d , h , ip ))
                end
                                          


          (* On a un signal s in t dans la chaîne de contrôle, on remplace la chaîne de contrôle (que l'on stock dans le dépôt) par t et on sauvegarde dans le dépôt le reste plus le signal *)
        | MachineSECD(id,Stack_signal signal::s,e,InitSignal::c,tl,(cs,ssi),d,h,ip)                   
          ->    begin
                  try   machineSECD (MachineSECD( id , s , e , c, tl , ((add_init_signal cs id signal),ssi) , d , h , ip ))
                  with  SignalAlreadyInit ->  machineSECD (MachineSECD( id , Stack_throw 9::s , e , c , tl , (cs,ssi) , d , h , ip ))
                end


          (* On a un present dans la chaîne de contrôle, on regarde si le signal est émit : si oui on prends la première possibilités sinon on le mets dans la liste de threads bloqués *)
        | MachineSECD(id,Stack_signal signal::s,e,Present(c1,c2)::c,tl,(cs,ssi),d,h,ip)               
            ->    if (isInit cs id signal)
                    then if (isEmit cs signal)
                            then machineSECD (MachineSECD( id , s , e , (append c1 c) , tl , (cs,ssi) , d , h , ip ))

                            else match tl with
                                    ([],st)                          
                                    ->    machineSECD (MachineSECD( ip , [] , [] , [] , ([],(append st [(signal,Thread(id,Stack_signal signal::s,e,Present(c1,c2)::c,d))])) , (cs,ssi) , Vide_D , h , (ip+1) ))

                                  | (Thread(id1,s1,e1,c3,d1)::w,st)  
                                    ->    machineSECD (MachineSECD( id1 , s1 , e1 , c3 , (w,(append st [(signal,Thread(id,Stack_signal signal::s,e,Present(c1,c2)::c,d))])) , (cs,ssi) , d1 , h , ip ))

                    else machineSECD (MachineSECD( id , Stack_throw 8::s , e , c , tl , (cs,ssi) , d , h , ip ))


          (* On a un emit dans la chaîne de contrôle, on réveil les threads bloqués en attente de ce signal et je le mets comme émit dans la liste des signaux *)
        | MachineSECD(id,Stack_signal signal::s,e,Emit::c,tl,(cs,ssi),d,h,ip)                         
          ->    begin 
                  try   machineSECD (MachineSECD( id , s , e , c , (check_thread_list signal tl) , ((emit_signal cs id signal),ssi) , d , h , ip))
                  with  SignalNotInit  ->   machineSECD (MachineSECD( id , Stack_throw 8::s , e , c , tl , (cs,ssi) , d , h , ip )) 
                end


          (* On a rien dans la chaîne de contrôle et le dépôt est vide mais la liste d'attente à au moins un élément, on prends un thread dans la liste d'attente *)                         
        | MachineSECD(id,s,e,[],(Thread(id1,s1,e1,c,d)::w,st),si,Vide_D,h,ip)     ->    machineSECD (MachineSECD( id1 , s1 , e1 , c , (w,st) , si , d , h , ip ))


          (* On a rien dans la chaîne de contrôle, le dépôt est vide et la liste d'attente aussi, 
             c'est la fin d'un instant où la fin du fonctionnement de la machine SECD si la liste de threads bloqués est vide *)
        | MachineSECD(id,s,e,[],([],st),si,Vide_D,h,ip)                           
          ->    begin
                  match (s,st) with
                      (v::s1,[]) 
                      ->    begin
                              match v with
          
                                | Stack_const b             ->   [Constant b]
          
                                | Stack_error e             ->   [Variable (error_message e)]
          
                                | Closure([Pair(x,c)],env)  ->   [Pair(x,c)]
          
                                | _                         ->   raise UnknowStackState
                            end

                    | _    ->    machineSECD (MachineSECD( id , s , e , [] , (end_of_the_moment_thread ([],st)) , (end_of_the_moment_signals si) , Vide_D , h , ip ))
                end


          (* On a Ap dans la chaîne de contrôle et on a un élément, on enlève Ap *)  
        | MachineSECD(id,v::s,e,Ap::c,tl,si,d,h,ip)                               ->    machineSECD (MachineSECD( id , v::s , e , c , tl , si , d , h , ip ))


          (* On a Ap dans la chaîne de contrôle, on enlève Ap *)  
        | MachineSECD(id,s,e,Ap::c,tl,si,d,h,ip)                                  ->    machineSECD (MachineSECD( id , s , e , c , tl , si , d , h , ip ))
         

          (* Je ne connais pas cette état ... *)
        | _                                                                       ->    raise StrangeEnd
  

    (* Lance et affiche le résultat de l'expression *)
    let lancerSECD expression =
       printf "Le résultat est %s \n" (string_of_control_string (machineSECD (MachineSECD(0,[],[],(secdLanguage_of_exprISWIM expression),([],[]),([],[]),Vide_D,Vide_H,1))))
    
  end