open String ;;
open Printf ;;
open List ;;
open Machine_cc.CCMachine ;;
open Machine_cek.CEKMachine ;;
open Lang_secdCv2.ISWIM ;;


module SECDCv2Machine =
  struct

    (**** Types ****)

    (* Types permettant une meilleure compréhension des autres types *)
    type id = string
    type variable = string
    type erreur = int

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
      | Throw of erreur
      | Catch of erreur * c list * (variable * c list)

    (* Type représentant la chaîne de contrôle *)
    type control_string = c list

    (* Type intermédiaire pour représenter l'environnement *)
    type e =  
        EnvFerm of string * (control_string * e list) 
      | EnvVar of string * int
      | Init of id                                      (* (init,s) *)

    (* Type représentant l'environnement *)
    type env = e list

    (* Type intermédiaire pour représenter la pile *)
    type s =  
        Fermeture of (control_string * env) 
      | Stack_const of int
      | Stack_throw of erreur
      | Remp

    (* Type représentant la pile *)
    type stack = s list

    (* Type représentant le dépôt *)
    type dump =
        Vide
      | Save of stack * env * control_string * dump


    (* Type représentant la liste de signaux émit *)
    type signals = id list


    (* Type représentant la liste des threads bloqués *)
    type stuck = (id * dump) list


    (* Type représentant la file des threads en attente *)
    type wait = dump list


    (* Type représentant le gestionnaire d'erreur *)
    type handler = 
        None
      | Handler of int * (stack * env * control_string * dump *  wait * stuck * signals * handler) 


    (* Type représentant la machine SECD concurrente version 1 *)
    type secdCv2 = Machine of stack * env * control_string * dump * wait * stuck * signals * handler



    (**** Exceptions ****)

    exception IllegalAddEnv
    exception FormatSpawnError
    exception SignalNotInit
    exception StrangeStuck
    exception UnknowWaitState


    (**** Affichage ****)

    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
          Const const                   ->   [Constant const]
            
        | Var var                       ->   [Variable var]
            
        | App(expr1,expr2)              ->   append (append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Op(op,liste_expr)             ->   append (flatten( map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Abs(abs,expr)                 ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]

        | Spawn_ISWIM(expr)             ->   append (append [Bspawn] (secdLanguage_of_exprISWIM expr)) [Espawn]
      
        | Present_ISWIM(s,expr1,expr2)  ->   [Present(s,(secdLanguage_of_exprISWIM expr1),(secdLanguage_of_exprISWIM expr2))]
      
        | Emit_ISWIM(s)                 ->   [Emit s]
     
        | Signal_ISWIM(s,expr)          ->   [Signal(s,(secdLanguage_of_exprISWIM expr))]

        | Throw_ISWIM erreur            ->   [Throw erreur]

        | Catch_ISWIM(erreur,expr1,(abs,expr2))  ->   [Catch(erreur,(secdLanguage_of_exprISWIM expr1),(abs,(secdLanguage_of_exprISWIM expr2)))]


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

        | Catch(erreur,expr1,(abs,expr2))::t  ->    "try "^(string_of_control_string expr1)^" catch "^(message_of_erreur erreur)
                                                        ^" in <"^abs^" , "^(string_of_control_string expr2)^"> "^(string_of_control_string t) 


    (* Convertit un environnement en chaîne de caractère *)
    let rec string_of_env env =
      match env with
          []                                    ->   ""

        | [EnvFerm(var,(control_string,env))]   ->   "<"^var^" ,<"^(string_of_control_string control_string)^","^(string_of_env env)^">>"

        | EnvFerm(var,(control_string,env))::t  ->   "<"^var^" ,<"^(string_of_control_string control_string)^","^(string_of_env env)^">> , "^(string_of_env t)

        | [EnvVar(var,const)]                   ->   "<"^var^" ,"^(string_of_int const)^">"

        | EnvVar(var,const)::t                  ->   "<"^var^" ,"^(string_of_int const)^"> , "^(string_of_env t)

        | [Init s]                              ->   "<"^s^",init>"

        | Init s::t                             ->   "<"^s^",init> , "^(string_of_env t)


    (* Convertit une pile en chaîne de caractère *)
    let rec string_of_stack stack =
      match stack with
          []                                    ->   ""

        | Fermeture(control_string,env)::t      ->   "["^(string_of_control_string control_string)^" , {"^(string_of_env env)^"}] "^(string_of_stack t)

        | Stack_const b::t                      ->   (string_of_int b)^" "^(string_of_stack t)

        | Stack_throw e::t                      ->   (message_of_erreur e)^" "^(string_of_stack t)

        | Remp::t                               ->   "Remp "^(string_of_stack t)   


    (* Convertit la sauvegarde en chaîne de caractère *)
    let rec string_of_dump dump =
      match dump with 
          Vide                                 ->   ""

        | Save(stack,env,control_string,dump)  ->   "( "^(string_of_stack stack)^" , ["^(string_of_env env)^"] , "^(string_of_control_string control_string)^" , "^(string_of_dump dump)^" )"

    
    (* Convertit la file d'attente en chaîne de caractère *)
    let rec string_of_wait wait = 
      match wait with
          []        ->   ""

        | [thread]  ->   (string_of_dump thread)

        | thread::t ->   (string_of_dump thread)^" , "^(string_of_wait t)

    
    (* Convertit la liste de threads bloqués en chaîne de caractère *)
    let rec string_of_stuck stuck =
      match stuck with
          []             ->   ""

        | [(s,thread)]   ->   "< "^s^","^(string_of_dump thread)^" >"

        | (s,thread)::t  ->   "< "^s^","^(string_of_dump thread)^" > , "^(string_of_stuck t)


    (* Convertit la liste des signaux émis en chaîne de caractère *)
    let rec string_of_signals signals =
      match signals with
          []    ->   ""

        | [s]   ->   s

        | s::t  ->   s^" , "^(string_of_signals t)

    
    (* Convertit le gestionnaire d'erreur en chaîne de caractères *)
    let rec string_of_handler handler =
      match handler with
          None                                                                ->   ""

        | Handler(erreur,(stack,env,control_string,dump,wait,stuck,si,handler))  -> 
                                       "\n   ERREUR  : "^(message_of_erreur erreur)
                                      ^"\n   STACK   : "^(string_of_stack stack)
                                      ^"\n   ENV     : "^(string_of_env env)
                                      ^"\n   CONTROL : "^(string_of_control_string control_string)
                                      ^"\n   DUMP    : "^(string_of_dump dump)
                                      ^"\n   WAIT    : "^(string_of_wait wait)
                                      ^"\n   STUCK   : "^(string_of_stuck stuck)
                                      ^"\n   SIGNALS : "^(string_of_signals si)
                                      ^"\n   HANDLER : "^(string_of_handler handler)
                                      ^" \n"


    (* Convertit une machine SECD concurrente version 2 en chaîne de caractère *)
    let rec string_of_Machine machine =
      match machine with
        Machine(stack,env,control_string,dump,wait,stuck,signals,handler)  ->
                                                                   "\n STACK   : "^(string_of_stack stack)
                                                                  ^"\n ENV     : ["^(string_of_env env)^"]"
                                                                  ^"\n CONTROL : "^(string_of_control_string control_string)
                                                                  ^"\n DUMP    : "^(string_of_dump dump)
                                                                  ^"\n WAIT    : "^(string_of_wait wait)
                                                                  ^"\n STUCK   : "^(string_of_stuck stuck)
                                                                  ^"\n SIGNALS : ["^(string_of_signals signals)^"]"
                                                                  ^"\n HANDLER : "^(string_of_handler handler)
                                                                  ^"\n"


    (* Affiche la machine SECD concurrente version 2 *)
    let afficherSECDCv2 machine = printf "MachineSECDCv2 : %s" (string_of_Machine machine)






    (**** Fonctions utiles ****)

    (* Substitue une variable à sa  fermeture liée *)
    let rec substitution x env =
      match env with
          []                         ->   raise AucuneSubPossible

        | EnvFerm(var,fermeture)::t  ->   if ( equal x var) then  Fermeture fermeture else substitution x t

        | EnvVar(var,b)::t           ->   if ( equal x var) then  Stack_const b else substitution x t

        | Init s::t                  ->   substitution x t


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

        | ([],Stack_throw e)               ->   [EnvVar(varARemp,e)]

        | ([],Fermeture(c,e))              ->   [EnvFerm(varARemp,(c,e))]

        | (EnvVar(v,b)::t,Fermeture f)     ->   if (equal v varARemp) then append [EnvFerm(v,f)] t else append [EnvVar(v,b)] (ajoutEnv t varARemp (Fermeture f))

        | (EnvFerm(v,f1)::t,Fermeture f)   ->   if (equal v varARemp) then append [EnvFerm(v,f)] t else append [EnvFerm(v,f1)] (ajoutEnv t varARemp (Fermeture f))

        | (EnvVar(v,b1)::t,Stack_const b)  ->   if (equal v varARemp) then append [EnvVar(v,b)] t else append [EnvVar(v,b1)] (ajoutEnv t varARemp (Stack_const b))

        | (EnvFerm(v,f)::t,Stack_const b)  ->   if (equal v varARemp) then append [EnvVar(v,b)] t else append [EnvFerm(v,f)] (ajoutEnv t varARemp (Stack_const b))

        | (EnvVar(v,b1)::t,Stack_throw e)  ->   if (equal v varARemp) then append [EnvVar(v,e)] t else append [EnvVar(v,b1)] (ajoutEnv t varARemp (Stack_throw e))

        | (EnvFerm(v,f)::t,Stack_throw e)  ->   if (equal v varARemp) then append [EnvVar(v,e)] t else append [EnvFerm(v,f)] (ajoutEnv t varARemp (Stack_throw e))

        | (Init s::t,v)                    ->   append [Init s] (ajoutEnv t varARemp v)

        | (_,_)                            ->   raise  IllegalAddEnv

    
    (* Retourne la chaîne de contrôle pour le nouveau thread et le reste *)
    let rec spawn c =
      match c with
          []          ->   raise FormatSpawnError

        | Espawn::c2  ->   ([],c2) 

        | h::t        ->   let (c1,c2) = spawn t in ((append [h] c1),c2)


    (* Ajoute une initialisation de signal *)
    let addInit env s = append env [Init s]


    (* Regarde dans l'environnement si le signal est initialisé *)
    let rec isInit env s = 
      match env with
          []          ->   false

        | Init s1::t  ->   if (equal s s1) then true else (isInit t s)

        | _::t        ->   isInit t s


    (* Prends le choix qui représente l'absence d'un signal *)
    let rec secondChoix st =
      match st with
          []                                                 ->   []

        | (signal,Save(s,e,Present(signal1,c1,c2)::c,d))::t  ->   append [Save(s,e,(append c2 c),d)] (secondChoix t)

        | _                                                  ->   raise StrangeStuck


    (* Emet un signal et vérifie si des threads sont en attente de cette émission *)
    let emit signal env st si =
      let rec aux st =
        match st with 
            []             ->   ([],[])

          | (s,thread)::t  ->   let (w1,st1) = aux t in if (equal signal s) then (append [thread] w1,st1) else (w1,append [(s,thread)] st1)
      in
      if (isInit env signal) then let (w1,st1) = aux st in (w1,st1,(append [signal] si)) else raise SignalNotInit


    (* Test si un signal est émis *)
    let isEmit env signal si = if (isInit env signal) then if(mem signal si) then true else false else raise SignalNotInit


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
    let lancerSECDCv2 expression afficher = printf "Le résultat est %s \n" (string_of_control_string (machineSECDCv2 (Machine([],[],(secdLanguage_of_exprISWIM expression),Vide,[],[],[],None)) afficher))

  end