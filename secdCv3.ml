open String ;;
open Printf ;;
open List ;;
open Cc.CCMachine ;;
open Cek.CEKMachine ;;
open LangISWIMCv3.ISWIM ;;


module SECDCv3Machine =
  struct

    (**** Types ****)

    (* Types permettant une meilleure compréhension des autres types *)
    type id = string
    type variable = string
    type erreur = int
    type id_thread = int
    type init = bool
    type emit = bool

    (* Type intermédiaire pour représenter la chaîne de contrôle *)
    type c =
        Constant of int 
      | Variable of string
      | Ap
      | Prim of operateur
      | Pair of string * c list
      | Bspawn                                   (* début du spawn *)
      | Espawn                                   (* fin du spawn *)
      | Emit of id                               (* emet s *)
      | Present of id * c list * c list          (* present s in t1 t2 *)
      | Signal of id * c list                    (* signal s in t *)
      | Erreur of erreur
      | Throw of erreur
      | Catch of erreur * c list * (variable * c list)
      | Put of id                                        (* place dans un signal      *)
      | Get of id                                        (* prend dans un signal     *)

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
      | Stack_erreur of erreur
      | Unit

    (* Type représentant la pile *)
    type stack = s list

    (* Type représentant le dépôt *)
    type dump =
        Vide
      | Save of stack * env * control_string * dump
      | SaveS of id * (stack * env * control_string * dump)


    (**** Thread list ****)

    (* type intermédiaire représentant un thread, comprenant son identifiant, sa pile , son environnement, sa chaîne de contrôle et son dépôt *)
    type thread = Thread of id_thread * stack * env * control_string * dump

    (* Ce type représente la file d'attente de la machine SECD version concurrente, c'est-à-dire, 
       la file contenant les threads qui doivent être traité dans l'instant courant *)
    type wait = thread list

    (* Ce type représente la file de thread bloqués de la machine SECD version concurrente, c'est-à-dire, 
       la file contenant les threads qui sont en attente d'un signal ou juste de la fin de l'instant courant *)
    type stuck = (id * thread) list

    (* Ce type représente l'ensemble des threads en cours de la machine SECD version concurrente. Cette ensemble est divisé en deux dans une pair, 
       d'un côté la liste d'attente de leurs tours et de l'autre la lite de threads bloqués *)
    type thread_list = wait * stuck



    (**** Signals ****)

    (* type intermédiaire contenant pour un id de thread donné, sa liste de signaux qui ont eux même 
       leurs liste de valeurs, un booléen qui représente leurs initialisations et un autre leurs émissions *)
    type cs = CS of id * (id_thread * int list * init) list * emit

    (* Ce type représente les signaux courants, c'est-à-dire, les signaux qui sont initialisé et émit à l'instant courant *)
    type current_signals = cs list

    (* type intermédiaire contenant pour un id de thread donné, sa liste de signaux qui ont eux mếme leurs liste de valeurs *)
    type ssi = SSI of id * ((id_thread * ((int * (id_thread list)) list) * id_thread list ) list)

    (* Ce type représente les signaux partagés, c'est-à-dire, les signaux qui ont été émit à l'instant précédent *)
    type shared_signals = ssi list

    (* Les signaux sont regroupés dans un type qui est une pair comprenant 
       les signaux courants et les signaux partagés qui sont les signaux de l'instant précédent *)
    type signals = current_signals * shared_signals

        (**** Identifier producer ****)

    (* Ce type représente un producteur d'identifiant de la machine SECD version concurrente, c'est-à-dire,
       un entier qui va donné un identifiant unique à chaque thread et s'incrémenter *)
       type identifier_producer = int


    (* Type représentant le gestionnaire d'erreur *)
    type handler = 
        None
      | Handler of int * (stack * env * control_string * dump *  thread_list * signals * handler * identifier_producer) 


    (* Type représentant la machine SECD concurrente version 3 *)
    type secdCv3 = Machine of stack * env * control_string * dump * thread_list * signals * handler * identifier_producer



    (**** Exceptions ****)

    exception IllegalAddEnv
    exception FormatSpawnError
    exception SignalNotInit
    exception StrangeStuck
    exception UnknowWaitState
    exception DataNotFound
    exception SignalAlreadyEmit


    (**** Affichage ****)

    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
          Const const                   ->   [Constant const]
            
        | Var var                       ->   [Variable var]
            
        | App(expr1,expr2)              ->   append (append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Op(op,liste_expr)             ->   append (flatten( map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Abs(abs,expr)                 ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]

        | Spawn(expr)                   ->   append (append [Bspawn] (secdLanguage_of_exprISWIM expr)) [Espawn]
      
        | Present_ISWIM(s,expr1,expr2)  ->   [Present(s,(secdLanguage_of_exprISWIM expr1),(secdLanguage_of_exprISWIM expr2))]
      
        | Emit_ISWIM(s)                 ->   [Emit s]
     
        | Signal_ISWIM(s,expr)          ->   [Signal(s,(secdLanguage_of_exprISWIM expr))]

        | Throw_ISWIM erreur            ->   [Throw erreur]

        | Catch_ISWIM(erreur,expr1,(abs,expr2))  ->   [Catch(erreur,(secdLanguage_of_exprISWIM expr1),(abs,(secdLanguage_of_exprISWIM expr2)))]

        | Put_ISWIM(id,valeur)          ->   [Constant valeur ; Put id]

        | Get_ISWIM(id,valeur)          ->   [Constant valeur ; Get id]


    (* Retourne un message par rapport à un identifiant d'erreur *)
    let message_of_erreur erreur =
      match erreur with
          1  ->   "ERREUR : Aucune substitution possible"                (* AucuneSubPossible *)
        | 2  ->   "ERREUR : Etat inconnu"                                (* Etatinconnu *)
        | 3  ->   "ERREUR : Format de l'opération erronée"               (* FormatOpErreur *)
        | 4  ->   "ERREUR : Format de l'environnement invalide"          (* IllegalAddEnv*)
        | 5  ->   "ERREUR : Format du spawn est erroné"                  (* FormatSpawnError *)
        | 6  ->   "ERREUR : Le sigbal n'est pas initialisé"              (* SignalNotInit *)
        | 7  ->   "ERREUR : Format de la file d'attente erronée"         (* UnknowWaitState *)
        | 8  ->   "ERREUR : Signal déjà initialisé"
        | 9  ->   "ERREUR : Format du catch invalide"
        | _  ->   "ERREUR : Cette erreur n'existe pas "


    (* Convertit la chaîne de contrôle en une chaîne de caractère *)
    let rec string_of_control_string expression =
      match expression with
          []                          ->   ""
      
        | Constant const::t           ->   (string_of_int const)^" "^(string_of_control_string t)
      
        | Variable var::t             ->   var^" "^(string_of_control_string t)
      
        | Ap::t                       ->   "ap "^(string_of_control_string t)
      
        | Pair(abs,liste_expr)::t     ->   "< "^abs^"."^(string_of_control_string liste_expr)^"> "^(string_of_control_string t)
      
        | Prim op::t                  ->   "prim "^(string_of_operateur op)^" "^(string_of_control_string t)

        | Bspawn::t                   ->   "bspawn "^(string_of_control_string t)
        
        | Espawn::t                   ->   "espawn "^(string_of_control_string t)
      
        | Emit s::t                   ->   s^" emit "^(string_of_control_string t)  
      
        | Present(s,expr1,expr2)::t   ->   "< "^s^","^(string_of_control_string expr1)^","^(string_of_control_string expr2)^" > "^(string_of_control_string t)  
        
        | Signal(s,expr)::t           ->   "< "^s^","^(string_of_control_string expr)^" > "^(string_of_control_string t)
      
        | Throw erreur::t                     ->   (message_of_erreur erreur)^" "^(string_of_control_string t)

        | Erreur erreur::t                     ->   (message_of_erreur erreur)^" "^(string_of_control_string t)

        | Catch(erreur,expr1,(abs,expr2))::t  ->    "try "^(string_of_control_string expr1)^" catch "^(message_of_erreur erreur)
                                                        ^" in <"^abs^" , "^(string_of_control_string expr2)^"> "^(string_of_control_string t) 

        | Put id::t               ->   "put in "^id^" "^(string_of_control_string t) 

        | Get id::t               ->   "get in "^id^" "^(string_of_control_string t) 


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
          []                                    ->   ""

        | Fermeture(control_string,env)::t      ->   "["^(string_of_control_string control_string)^" , {"^(string_of_env env)^"}] "^(string_of_stack t)

        | Stack_const b::t                      ->   (string_of_int b)^" "^(string_of_stack t)

        | Stack_erreur e::t                     ->   (message_of_erreur e)^" "^(string_of_stack t)

        | Unit::t                               ->   "Unit "^(string_of_stack t)   


    (* Convertit la sauvegarde en chaîne de caractère *)
    let rec string_of_dump dump =
      match dump with 
          Vide                                 ->   ""

        | Save(stack,env,control_string,dump)  ->   "( "^(string_of_stack stack)^" , ["^(string_of_env env)^"] , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )"

        | SaveS(id,(stack,env,control_string,dump))  ->   "("^id^",( "^(string_of_stack stack)^" , ["^(string_of_env env)^"] , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" ))"


    (* Convertit un thread en chaîne de caractère *)
    let string_of_thread thread = 
      match thread with
        Thread(i,s,e,c,d)  ->   "( "^(string_of_int i)^" , "^(string_of_stack s)^" , ["^(string_of_env e)^"] , "^(string_of_control_string c)^" , "^(string_of_dump d)^" )" 
    

    (* Convertit la file d'attente en chaîne de caractère *)
    let rec string_of_wait wait = 
      match wait with
          []        ->   ""

        | [thread]  ->   (string_of_thread thread)

        | thread::t ->     (string_of_thread thread)^", "^(string_of_wait t)

    
    (* Convertit la liste de threads bloqués en chaîne de caractère *)
    let rec string_of_stuck stuck =
      match stuck with
          []             ->   ""

        | [(s,thread)]   ->   "< "^s^","^(string_of_thread thread)^" >"

        | (s,thread)::t  ->   "< "^s^","^(string_of_thread thread)^" > , "^(string_of_stuck t)

    let string_of_thread_list thread_list =
      match thread_list with
        (wait,stuck)  ->    "\n    WAIT    : "^(string_of_wait wait)
                           ^"\n    STUCK   : "^(string_of_stuck stuck)


    (* Convertit une liste de signaux courant en chaîne de caractères *)
    let rec string_of_cs cs_list =
      match cs_list with
          []                               ->   ""

        | (id_thread,values,init)::t       ->    "("^(string_of_int id_thread)^",{"^(concat_string_liste(map string_of_int values))^"},"^(string_of_bool init)^") "^(string_of_cs t)


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

          | [(value,pointers)]                 ->   "("^( string_of_int value)^",{"^(concat_string_liste(map string_of_int pointers))^"})"

          | (value,pointers)::t                ->   "("^( string_of_int value)^",{"^(concat_string_liste(map string_of_int pointers))^"});"^(aux t)
      in
      match ssi_list with
          []                                   ->   ""

        | (id_thread,values,thread_list)::t    ->   " ("^(string_of_int id_thread)^",["^(aux values)^"],{"^(concat_string_liste( map string_of_int thread_list))^"}) "^(string_of_ssi t)

    
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

    
    (* Convertit le gestionnaire d'erreur en chaîne de caractères *)
    let rec string_of_handler handler =
      match handler with
          None                                                                       ->   ""

        | Handler(erreur,(stack,env,control_string,dump,thread_list,si,handler,ip))  -> 
                                       "\n   ERREUR  : "^(message_of_erreur erreur)
                                      ^"\n   STACK   : "^(string_of_stack stack)
                                      ^"\n   ENV     : "^(string_of_env env)
                                      ^"\n   CONTROL : "^(string_of_control_string control_string)
                                      ^"\n   DUMP    : "^(string_of_dump dump)
                                      ^"\n   THREADS : "^(string_of_thread_list thread_list)
                                      ^"\n   SIGNALS : "^(string_of_signals si)
                                      ^"\n   HANDLER : "^(string_of_handler handler)
                                      ^"\n   IP      : "^(string_of_int ip)
                                      ^" \n"


    (* Convertit une machine SECD concurrente version 3 en chaîne de caractère *)
    let rec string_of_Machine machine =
      match machine with
        Machine(stack,env,control_string,dump,thread_list,signals,handler,ip)  ->
                                                                   "\n STACK   : "^(string_of_stack stack)
                                                                  ^"\n ENV     : ["^(string_of_env env)^"]"
                                                                  ^"\n CONTROL : "^(string_of_control_string control_string)
                                                                  ^"\n DUMP    : "^(string_of_dump dump)
                                                                  ^"\n THREADS : "^(string_of_thread_list thread_list)
                                                                  ^"\n SIGNALS : ["^(string_of_signals signals)^"]"
                                                                  ^"\n HANDLER : "^(string_of_handler handler)
                                                                  ^"\n IP      : "^(string_of_int ip)
                                                                  ^"\n"


    (* Affiche la machine SECD concurrente version 3 *)
    let afficherSECDCv3 machine = printf "MachineSECDCv3 : %s" (string_of_Machine machine)






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

        | ([],Stack_erreur e)              ->   [EnvVar(varARemp,e)]

        | ([],Fermeture(c,e))              ->   [EnvFerm(varARemp,(c,e))]

        | (EnvVar(v,b)::t,Fermeture f)     ->   if (equal v varARemp) then append [EnvFerm(v,f)] t else append [EnvVar(v,b)] (ajoutEnv t varARemp (Fermeture f))

        | (EnvFerm(v,f1)::t,Fermeture f)   ->   if (equal v varARemp) then append [EnvFerm(v,f)] t else append [EnvFerm(v,f1)] (ajoutEnv t varARemp (Fermeture f))

        | (EnvVar(v,b1)::t,Stack_const b)  ->   if (equal v varARemp) then append [EnvVar(v,b)] t else append [EnvVar(v,b1)] (ajoutEnv t varARemp (Stack_const b))

        | (EnvFerm(v,f)::t,Stack_const b)  ->   if (equal v varARemp) then append [EnvVar(v,b)] t else append [EnvFerm(v,f)] (ajoutEnv t varARemp (Stack_const b))

        | (EnvVar(v,b1)::t,Stack_erreur e) ->   if (equal v varARemp) then append [EnvVar(v,e)] t else append [EnvVar(v,b1)] (ajoutEnv t varARemp (Stack_erreur e))

        | (EnvFerm(v,f)::t,Stack_erreur e) ->   if (equal v varARemp) then append [EnvVar(v,e)] t else append [EnvFerm(v,f)] (ajoutEnv t varARemp (Stack_erreur e))

        | (_,_)                            ->   raise  IllegalAddEnv

    
    (* Retourne la chaîne de contrôle pour le nouveau thread et le reste *)
    let rec spawn c =
      match c with
          []          ->   raise FormatSpawnError

        | Espawn::c2  ->   ([],c2) 

        | h::t        ->   let (c1,c2) = spawn t in ((append [h] c1),c2)


    let prendreDataSignalCS thread s si =
      let rec aux cs_list = 
        match cs_list with
            []                   ->   raise DataNotFound

          | CS(id,data,_)::t  ->   if (equal id s) then data else aux t
      in
      match si with
        (cs,ssi) -> aux cs


    (* Regarde dans l'environnement si le signal est initialisé *)
    let isInit thread s si = 
      let rec aux1 data =
        match data with
            []              ->   false
           
          | (th,_,init)::t  ->   if (th = thread) then init else aux1 t
      in
      try  aux1 (prendreDataSignalCS thread s si)
      with DataNotFound -> false


      (* Test si un signal est émis *)
    let isEmit s si = 
      let rec aux cs_list = 
        match cs_list with
            []                   ->   raise SignalNotInit

          | CS(id,_,emit)::t  ->   if (equal id s) then emit else aux t
      in
      match si with
        (cs,ssi) -> aux cs


    (* Prends le choix qui représente l'absence d'un signal *)
    let rec secondChoix st =
      match st with
          []                                                 ->   []

        | (signal,Save(s,e,Present(signal1,c1,c2)::c,d))::t  ->   append [Save(s,e,(append c2 c),d)] (secondChoix t)

        | _                                                  ->   raise StrangeStuck


    (* Emet un signal et vérifie si des threads sont en attente de cette émission *)
    let emit signal st si =
      let rec aux1 st =
        match st with 
            []             ->   ([],[])

          | (s,thread)::t  ->   let (w1,st1) = aux1 t in if (equal signal s) then (append [thread] w1,st1) else (w1,append [(s,thread)] st1)
      in
      let rec aux cs_list = 
        match cs_list with
            []                    ->   raise SignalNotInit

          | CS(id,data,true)::t   ->   if (equal id signal) then raise SignalAlreadyEmit else (append [CS(id,data,true)] (aux t))

          | CS(id,data,false)::t  ->   if (equal id signal) then (append [CS(id,data,true)] t) else (append [CS(id,data,false)] (aux t))
      in
      match si with
        (cs,ssi) -> let (w1,st1) = aux1 st in (w1,st1,(aux cs,ssi))



    (**** Machine SECD concurrente version 2 ****)

    (* Applique une transition de la machine SECD concurrente version 2 pour un état donné *)
    let transitionSECDCv2 machine =
      match machine with

          (* Traitement erreur *)                                                                            
          | Machine(Stack_throw erreur::s,e,c,d,w,st,si,Handler(erreur1,(s1,e1,Pair(abs,c1)::c2,d1,w1,st1,si1,h)))                
          ->  if (erreur = erreur1) then Machine([],(ajoutEnv e1 abs (Stack_throw erreur)),c1,Save(s1,e1,c2,d1),w1,st1,si1,h)
                                    else Machine(Stack_throw erreur::s,e,c,d,w,st,si,h)            

          (* Erreur non traitée *)
        | Machine(Stack_throw erreur::s,e,c,d,w,st,si,None)                ->   Machine([Stack_throw erreur],[],[],Vide,[],[],[],None)


          (* Constante *)
        | Machine(s,e,Constant b::c,d,w,st,si,h)                           ->   Machine(Stack_const b::s,e,c,d,w,st,si,h)
        
          (* Substitution *)
        | Machine(s,e,Variable x::c,d,w,st,si,h)                           ->   begin try Machine((substitution x e)::s,e,c,d,w,st,si,h)
                                                                                      with AucuneSubPossible -> Machine(Stack_throw 1::s,e,c,d,w,st,si,h)
                                                                                end

          (* Abstraction *)                                                                      
        | Machine(s,e,Pair(abs,expr)::c,d,w,st,si,h)                       ->   Machine(Fermeture([Pair(abs,expr)],e)::s,e,c,d,w,st,si,h)

          (* Opération *)
        | Machine(s,e,Prim op::c,d,w,st,si,h)                              ->   begin
                                                                                  try 
                                                                                      let (liste_entier,new_stack) = prendre_entier s (getNbrOperande op) in 
                                                                                      let res = (secdLanguage_of_exprISWIM (calcul op liste_entier)) in
                                                                                      match res with
                                                                                          [Constant b] ->  Machine(Stack_const b::new_stack,e,c,d,w,st,si,h)

                                                                                        | [Pair(abs,c1)] -> Machine(Fermeture([Pair(abs,c1)],e)::new_stack,e,c,d,w,st,si,h)

                                                                                        | _ -> Machine(Stack_throw 2::s,e,c,d,w,st,si,h) 
                                                                                  with FormatOpErreur -> Machine(Stack_throw 3::s,e,c,d,w,st,si,h) 
                                                                              end

          (* Application neutre droite *)                                                                    
        | Machine(v::Remp::s,e,Ap::c,d,w,st,si,h)                          ->   Machine(v::s,e,c,d,w,st,si,h)
        
          (* Application neutre gauche *)
        | Machine(Remp::v::s,e,Ap::c,d,w,st,si,h)                          ->   Machine(v::s,e,c,d,w,st,si,h)

          (* Application *)                                                                    
        | Machine(v::Fermeture([Pair(abs,c1)],e1)::s,e,Ap::c,d,w,st,si,h)  ->   begin try Machine([],(ajoutEnv e1 abs v),c1,Save(s,e,c,d),w,st,si,h)
                                                                                      with IllegalAddEnv -> Machine(Stack_throw 4::s,e,c,d,w,st,si,h) 
                                                                                end
          (* Récupération de sauvegarde *)
        | Machine(v::s,e,[],Save(s1,e1,c,d),w,st,si,h)                     ->   Machine(v::s1,e1,c,d,w,st,si,h)

          (* Création de thread *)
        | Machine(s,e,Bspawn::c,d,w,st,si,h)                               ->   begin try let (c1,c2) = spawn c in Machine(Remp::s,e,c2,d,(append w [Save(s,e,c1,d)]),st,si,h)
                                                                                      with FormatSpawnError -> Machine(Stack_throw 5::s,e,c,d,w,st,si,h)  
                                                                                end
          (* Initialisation signal *)
        | Machine(s,e,Signal(signal,c1)::c,d,w,st,si,h)                    ->   let e1 = addInit e signal in Machine([],e1,c1,Save(s,e,c,d),w,st,si,h)

          (* Emission *)                                                                    
        | Machine(s,e,Emit signal::c,d,w,st,si,h)                          ->   begin try let (w1,st1,si1) = emit signal e st si in Machine(Remp::s,e,c,d,w1,st1,si1,h)
                                                                                      with SignalNotInit -> Machine(Stack_throw 6::s,e,c,d,w,st,si,h)  
                                                                                end
          (* Présence d'un signal *)
        | Machine(s,e,Present(signal,c1,c2)::c,d,w,st,si,h)                ->   begin try 
                                                                                        if (isEmit e signal si) 
                                                                                          then Machine(s,e,(append c1 c),d,w,st,si,h)
                                                                                          else begin
                                                                                                match w with 
                                                                                                    []                    
                                                                                                    ->   Machine([],[],[],Vide,[],(append st [(signal,Save(s,e,Present(signal,c1,c2)::c,d))]),si,h)

                                                                                                  | Save(s1,e1,c3,d1)::t  
                                                                                                    ->   Machine(s1,e1,c3,d1,t,(append st [(signal,Save(s,e,Present(signal,c1,c2)::c,d))]),si,h)

                                                                                                  | _  ->   Machine(Stack_throw 7::s,e,c,d,w,st,si,h)  
                                                                                              end
                                                                                      with SignalNotInit ->  Machine(Stack_throw 6::s,e,c,d,w,st,si,h)  
                                                                                end
         
          (* Erreur *)                                                                           
        | Machine(s,e,Throw erreur::c,d,w,st,si,h)                         ->   Machine(Stack_throw erreur::s,e,c,d,w,st,si,h)
     
          (* Création gestionnaire d'erreur *)
        | Machine(s,e,Catch(erreur,c1,(abs,c2))::c,d,w,st,si,h)            ->   Machine(s,e,(append c1 c),d,w,st,si,Handler(erreur,(s,e,(append [Pair(abs,c2)] c),d,w,st,si,h)))
                                          
          (* Récupération de thread *)
        | Machine(s,e,[],Vide,Save(s1,e1,c,d)::w,st,si,h)                  ->   Machine(s1,e1,c,d,w,st,si,h)

          (* Fin d'un instant logique *)                                                                           
        | Machine(s,e,[],Vide,[],st,si,h)                                  ->   let w = secondChoix st in Machine(s,e,[],Vide,w,[],[],h)

        | _                                                                ->   raise EtatInconnu


    (* Applique les règles de la machine SECD concurrente version 2 en affichant les étapes *)
    let rec machineSECDCv2 machine afficher= 
      match machine with
          Machine([Stack_const b],e,[],Vide,[],[],si,h)                ->   [Constant b]
        
        | Machine([Fermeture([Pair(abs,c)],e1)],e,[],Vide,[],[],si,h)  ->   [Pair(abs,c)]

        | Machine([Stack_throw erreur],e,[],Vide,[],[],si,h)           ->   [Throw erreur]

        | machine                                                      ->   if (afficher) then (afficherSECDCv2 machine) else printf ""; machineSECDCv2 (transitionSECDCv2 machine) afficher

        
    (* Lance et affiche le résultat de l'expression *)
    let lancerSECDCv3 expression afficher = printf "Le résultat est %s \n" (string_of_control_string (machineSECDCv3 (Machine([],[],(secdLanguage_of_exprISWIM expression),Vide,[],[],[],None)) afficher))

  end