open String ;;
open Printf ;;
open List ;;
open Lang_ttsi.ISWIM ;;


module MachineTTSI =
  struct

    (**** Types ****)

    (* Petits types très pratique pour ne pas se mélanger dans la compréhension des types suivants *)
    type id_signal  =   int
    type variable   =   string
    type id_thread  =   int
    type emit       =   bool




    (**** Control string ****)

    (* type intermédiaire qui va servir à représenter la chaîne de contrôle *)
    type c =
        Constant of int                                         (* constante b                    *)
      | Variable of variable                                    (* variable X                     *)
      | Pair of variable * c list                               (* abstraction                    *)

      | Prim of operateur                                       (* opérateur                      *)
      | Ap                                                      (* application                    *)
      | Spawn                                                   (* création thread                *)
      | Init                                                    (* initialise un signal           *)
      | Emit                                                    (* émet un signal                 *)
      | Put                                                     (* place dans un signal           *)
      | Get                                                     (* prends dans un signal          *)
      | Present                                                 (* test de présence d'un signal   *)
      | Fix
      
     

    (* Ce type représente la chaîne de contrôle de la machineTTSI SECD, c'est notre entrée *)
    type control_string = c list   


    

    (**** Environment ****)

    (* type intermédiaire qui va servir à représenter l'environnement *)
    type e =   
        EnvClos of (variable * (control_string * e list))     (* (X,(C,Env))            *)
      | EnvVar  of (variable * control_string)                (* (X,V) V une constante  *)
      | EnvRec  of (variable * control_string)                (* (X,C) une récursion    *)

    (* Ce type représente l'environnement de la machineTTSI SECD, c'est notre liste de substitution *)
    type environment = e list




    (**** Stack ****)

    (* type intermédiaire contenant une fermeture qui lie une abstraction à un environnement ou une constante ou encore un signal *)
    type s =  
        Stack_const of int                                    (* constante            *)
      | Closure of (control_string * environment)             (* fermeture (C,Env)    *)

    
    (* Ce type représente la pile de la machineTTSI SECD, c'est la où la machineTTSI travaille *)
    type stack = s list




    (**** Dump ****)

    (* Ce type représente le dépôt de la machineTTSI SECD, c'est-à-dire, l'endroit où l'on sauvegarde l'état 
       de la machineTTSI SECD pour travailler sur une autre partie de la chaîne de contrôle *)
    type dump =
        Empty                                                        (* le dépôt est vide    *)
      | Save of stack * environment * control_string * dump          (* (s,e,c,d)            *)




    (**** Thread list ****)

    (* type intermédiaire représentant un thread, comprenant son identifiant, sa pile , son environnement, sa chaîne de contrôle et son dépôt *)
    type thread = Thread of id_thread * stack * environment * control_string * dump

    (* Ce type représente la file d'attente de la machineTTSI , c'est-à-dire, 
       la file contenant les threads qui doivent être traité dans l'instant courant *)
    type thread_list = thread list




    (**** Signals ****)

    (* type intermédiaire représentant la liste des valeurs courante d'un signal par rapport à un identifiant de thread *)
    type cs = (id_thread * int list) list 
   
    (* type intermédiaire représentant la liste des valeurs partagées *)
    type ci = (int * (id_thread list)) list

    (* type intermédiaire représentant la liste de valeurs partagées par rapport à un identifiant de thread *)
    type ssi = (id_thread * ci * id_thread list ) list
   
    (* type intermédiaire représentant toute les informations liées à un signal *)
    type data = bool * cs * ssi * thread_list
   
    (* type intermédiaire représentant un signal *)
    type si = id_signal * data
   
    (* Ce type représente la liste des signaux de la machineTTSI *)
    type signals = si list
   
    


    (**** Identifier producer ****)

    (* Ce type représente un producteur d'identifiant de la machineTTSI, c'est-à-dire,
       un entier qui va donné un identifiant unique à chaque thread et s'incrémenter *)
    type identifier_producer = int


    

    (**** MachineTTSI ****)

    (* Ce type représente la structure de la machineTTSI *)
    type ttsi = MachineTTSI of thread * thread_list * signals * identifier_producer 










    (**** Exception ****)

    exception NoSubPossible                    (* Aucune substitution possible pour cette variable dans l'environnement             *)
    exception StrangeEnd                       (* Même moi je ne sais pas ce qu'il sait passé ...                                   *)

    exception NotAllConstants                  (* Tous les éléments de la pile prisent pour l'opérateurs ne sont pas des constantes *)
    exception InsufficientOperandNb            (* Le nombre d'opérande est insuffisante par rapport au nombre requis                *)
    exception DivZero

    exception SignalNotInit
    exception SignalAlreadyEmit
    exception ThreadSharedNotFound             (* L'identifiant de thread lié à un signal n'existe pas                              *)

    exception UnknowStackState                 (* Le format de la pile est invalide et/ou inconnu                                   *)
    exception UnknowEnvState                   (* Le format de l'environnement est invalide et/ou inconnu                           *)
    exception UnknowStuckState                 (* Le format de la liste d'élément bloqués est invalide et/ou inconnu                *)
    exception UnknowSignalState                   (* Le format de la liste de signaux partagés est invalide                         *)

    exception BadVersion



    (**** Affichage ****)

    (* Concatène une liste de chaîne de caractères en une seule chaîne de caractères *)
    let rec string_of_string_list string_list =
      match string_list with
          []    ->   "" 

        | [h]     ->   h
        
        | h::t  ->   h^";"^(string_of_string_list t)

    
    (* Convertit une liste d'entier en une chaîne de caractères *)
    let string_of_int_list int_list = string_of_string_list (map string_of_int int_list)


    (* Convertit le langage ISWIM en langage SECD *)
    let rec secdLanguage_of_exprISWIM expression =
      match expression with
          Lang_ttsi.ISWIM.Const const                           ->   [Constant const]
          
        | Lang_ttsi.ISWIM.Var var                               ->   [Variable var]
            
        | Lang_ttsi.ISWIM.App(expr1,expr2)                      ->   append (append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Lang_ttsi.ISWIM.Op(op,liste_expr)                     ->   append (flatten(map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Lang_ttsi.ISWIM.Abs(abs,expr)                         ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]

        | Lang_ttsi.ISWIM.Spawn expr                      ->   [Pair("",(secdLanguage_of_exprISWIM expr)) ; Spawn]

        | Lang_ttsi.ISWIM.Present (signal,expr1,expr2)    ->   [Variable signal ; Pair("",(secdLanguage_of_exprISWIM expr1)) ; Pair("",(secdLanguage_of_exprISWIM expr2)) ; Present]

        | Lang_ttsi.ISWIM.Signal                          ->   [Init]

        | Lang_ttsi.ISWIM.Emit signal                    ->   [Variable signal; Emit] 

        | Lang_ttsi.ISWIM.Put(signal,value)               ->   [Constant value ; Variable signal ; Put]

        | Lang_ttsi.ISWIM.Get(signal,id_thread,neutral)   ->   [Variable signal ; Variable id_thread ; Constant neutral ; Get]

        | Lang_ttsi.ISWIM.Wait                                  ->   [Constant(-1) ; Pair("",[]) ; Pair("",[]) ; Present]

        | Lang_ttsi.ISWIM.Rec(f,t)                              ->   [Pair(f,(secdLanguage_of_exprISWIM t)) ; Fix]

        | Lang_ttsi.ISWIM.If(expr1,expr2,expr3)                 ->   (append ( append [ Pair("v",[Pair("t",[Pair("f1",[ Variable "v" ; Variable "t" ; Ap ; Variable "f1" ; Ap])])])]   (secdLanguage_of_exprISWIM expr1) )  [Ap ; Pair("",(secdLanguage_of_exprISWIM expr2)) ; Ap ; Pair("",(secdLanguage_of_exprISWIM expr3)) ; Ap])
    
        | _ -> raise BadVersion


    (* Convertit la chaîne de contrôle en une chaîne de caractères *)
    let rec string_of_control_string expression =
      match expression with

          []                                 ->   ""
      
        | Constant const::t                  ->   (string_of_int const)^" "^(string_of_control_string t)
      
        | Variable var::t                    ->   var^" "^(string_of_control_string t)
      
        | Ap::t                              ->   "ap "^(string_of_control_string t)
      
        | Pair(abs,expr_list)::t             ->   "<"^abs^", "^(string_of_control_string expr_list)^"> "^(string_of_control_string t)
      
        | Prim(op)::t                        ->   "prim "^(string_of_operateur op)^" "^(string_of_control_string t)

        | Spawn::t                           ->   "spawn "^(string_of_control_string t)

        | Present::t                         ->   "present "^(string_of_control_string t)

        | Init::t                            ->   "init "^(string_of_control_string t)

        | Emit::t                            ->   "emit "^(string_of_control_string t)

        | Put::t                             ->   "put "^(string_of_control_string t)
       
        | Get::t                             ->   "get "^(string_of_control_string t)

        | Fix::t                             ->   "fix "^(string_of_control_string t)


    (* Convertit un environnement en chaîne de caractères *)
    let rec string_of_environment environment =
      match environment with
          []                                      ->   ""
            
        | [(EnvClos(var,(control_string,env)))]   ->   " Closure : ["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_environment env)^"]]"

        | [(EnvVar(var,control_string))]          ->   " Var : ["^var^" , "^(string_of_control_string control_string) ^"]"

        | [(EnvRec(var,control_string))]          ->   " Rec : ["^var^" , "^(string_of_control_string control_string) ^"]"

        | (EnvClos(var,(control_string,env)))::t  ->   " Closure : ["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_environment env)^"]] , "^(string_of_environment t)

        | (EnvVar(var,control_string))::t         ->   " Var : ["^var^" , "^(string_of_control_string control_string) ^"] , "^(string_of_environment t)

        | (EnvRec(var,control_string))::t         ->   " Rec : ["^var^" , "^(string_of_control_string control_string) ^"] , "^(string_of_environment t)


    (* Convertit une pile en chaîne de caractères *)
    let rec string_of_stack stack =
      match stack with
          []                                ->   ""

        | Stack_const b::t                  ->   (string_of_int b)^" "^(string_of_stack t)

        | (Closure(control_string,env))::t  ->   "["^(string_of_control_string control_string)^" , {"^(string_of_environment env)^"}]"^(string_of_stack t)
        

    (* Convertit la sauvegarde en chaîne de caractères *)
    let rec string_of_dump dump =
      match dump with 
          Empty                                      ->   ""

        | Save(stack,env,control,dump)               ->    "("^(string_of_stack stack)^" , "^(string_of_environment env)^" , "^(string_of_control_string control)^" , "^(string_of_dump dump)^")"
        
      
    (* Convertit un thread en chaîne de caractères *)
    let rec string_of_thread thread =
      match thread with
        Thread(id,stack,env,control_string,dump)  ->    
                                        "\n     ID      : "^(string_of_int id)
                                       ^"\n     STACK   : "^(string_of_stack stack)
                                       ^"\n     ENV     : "^(string_of_environment env)
                                       ^"\n     CONTROL : "^(string_of_control_string control_string)
                                       ^"\n     DUMP    : "^(string_of_dump dump)
                                      

    (* Convertit la liste des threads en chaîne de caractères *)
    let rec string_of_thread_list thread_list =
      match thread_list with
          []         ->   ""

        | [Thread(id,stack,env,control_string,dump)]    ->    
                                                       "\n   [ ID      : "^(string_of_int id)
                                                      ^"\n     STACK   : "^(string_of_stack stack)
                                                      ^"\n     ENV     : "^(string_of_environment env)
                                                      ^"\n     CONTROL : "^(string_of_control_string control_string)
                                                      ^"\n     DUMP    : "^(string_of_dump dump)^"]\n"

        | Thread(id,stack,env,control_string,dump) ::t  ->    " 
                                                        \n   [ ID      : "^(string_of_int id)
                                                      ^"\n     STACK   : "^(string_of_stack stack)
                                                      ^"\n     ENV     : "^(string_of_environment env)
                                                      ^"\n     CONTROL : "^(string_of_control_string control_string)
                                                      ^"\n     DUMP    : "^(string_of_dump dump)^"]"
                                                      ^(string_of_thread_list t)

    
    (* Convertit la file d'attente en chaîne de caractères *)
    let rec string_of_cs cs =
      match cs with
          []              ->   ""

        | (id,values)::t  ->   "("^(string_of_int id)^" , {"^(string_of_int_list values)^"}) "^(string_of_cs t)
        

    (* Convertit une liste de valeurs avec pointeurs en chaîne de caractères *)
    let rec string_of_ci ci =
      match ci with
          []                  ->   ""

        | (value,id_list)::t  ->   "("^(string_of_int value)^" , {"^(string_of_int_list id_list)^"}) "^(string_of_ci t)


    (* Convertit la liste des signaux partagés en chaîne de caractères *)
    let rec string_of_ssi ssi = 
      match ssi with
          []                               ->   ""

        | (id_thread,ci,thread_list)::t    ->   " ("^(string_of_int id_thread)^",["^(string_of_ci ci)^"],{"^(string_of_int_list thread_list)^"}) "^(string_of_ssi t)


    (* Convertit toutes les informations sur un signal en chaîne de caractères *)
    let string_of_data data =
      match data with
        (emit,cs,ssi,tl)  ->  (string_of_bool emit)^" , "^(string_of_cs cs)^" , "^(string_of_ssi ssi)^" , "^(string_of_thread_list tl) 
    

    (* Convertit un signal en chaîne de caractères *)
    let string_of_si si =
      match si with
        (id_signal,data)  ->  "     ("^(string_of_int id_signal)^" : "^(string_of_data data)^")"


    (* Convertit la liste de tous les signaux en chaîne de caractères *)
    let rec string_of_signals signals =
      match signals with
          []     ->   ""      

        | si::t  ->   "\n"^(string_of_si si)^(string_of_signals t)


    (* Convertit une machine TTSI en chaîne de caractères *)
    let rec string_of_machineTTSI machineTTSI =
      match machineTTSI with
        MachineTTSI(thread,thread_list,signals,identifier_producer)  ->    
                                        "\n   THREAD  : "^(string_of_thread thread)
                                       ^"\n   THREADS : "^(string_of_thread_list thread_list)
                                       ^"\n   SIGNALS : "^(string_of_signals signals)
                                       ^"\n   IP      : "^(string_of_int identifier_producer)
                                       ^"\n"


    (* Affiche la machine TTSI *)
    let afficherTTSI machineTTSI = printf "MachineTTSI : %s\n" (string_of_machineTTSI machineTTSI)










    (**** Fonctions utiles ****)


    (* Substitue une variable à sa  fermeture liée *)
    let rec substitution x env =
      match env with
          []                                    ->   raise NoSubPossible

        | EnvClos(var,(control_string,env))::t  ->   if (equal x var) then  Closure(control_string,env) else substitution x t

        | EnvVar(var,control_string)::t         ->   if (equal x var) 
                                                      then  match control_string with
                                                                [Constant b]  ->   Stack_const b 

                                                              | _             ->   raise UnknowEnvState

                                                      else substitution x t

        | EnvRec(var,control_string)::t         ->   if (equal x var) then Closure(control_string,[EnvRec(var,control_string)]) else substitution x t

                                                      
    (* Ajoute une  fermeture à l'environnement *)
    let rec add_env env varToRep stack_element recursion =
      match stack_element with
          Stack_const b                 ->    begin
                                                match env with
                                                    [] -> [EnvVar(varToRep,[Constant b])]

                                                  | EnvClos(var1,closure)::t -> if (equal var1 varToRep) then append [EnvVar(varToRep,[Constant b])] t 
                                                                                                         else append [EnvClos(var1,closure)] (add_env t varToRep stack_element recursion)

                                                  | EnvVar(var1,control_string)::t -> if (equal var1 varToRep) then append [EnvVar(varToRep,[Constant b])] t 
                                                                                                               else append [EnvVar(var1,control_string)] (add_env t varToRep stack_element recursion)

                                                  | EnvRec(var1,control_string)::t -> if (equal var1 varToRep) then append [EnvVar(varToRep,[Constant b])] t 
                                                                                                               else append [EnvRec(var1,control_string)] (add_env t varToRep stack_element recursion)
                                              end

        | Closure(control_string,env1)  ->   begin
                                              match env with
                                                    [] -> if recursion then [EnvRec(varToRep,control_string)] else [EnvClos(varToRep,(control_string,env1))]

                                                  | EnvClos(var1,closure)::t -> if (equal var1 varToRep) then if recursion then append [EnvRec(varToRep,control_string)] t 
                                                                                                                           else append [EnvClos(varToRep,(control_string,env1))] t 
                                                                                                         else append [EnvClos(var1,closure)] (add_env t varToRep stack_element recursion)

                                                  | EnvVar(var1,control_string)::t -> if (equal var1 varToRep) then if recursion then append [EnvRec(varToRep,control_string)] t 
                                                                                                                                  else append [EnvClos(varToRep,(control_string,env1))] t 
                                                                                                                else append [EnvVar(var1,control_string)] (add_env t varToRep stack_element recursion)

                                                  | EnvRec(var1,control_string)::t -> if (equal var1 varToRep) then if recursion then append [EnvRec(varToRep,control_string)] t 
                                                                                                                                 else append [EnvClos(varToRep,(control_string,env1))] t  
                                                                                                               else append [EnvRec(var1,control_string)] (add_env t varToRep stack_element recursion)
                                              end

                
    (* Vérifie si le signal est émit *)
    let rec isEmit si signal = 
      let aux data =
        match data with
            (true,_,_,_)  ->   true

          | _             ->   false
      in
      match si with
          []              ->   false

        | (id_s,data)::t  ->   if (id_s = signal) then (aux data) else (isEmit t signal)


    (* Initialise un signal *)
    let rec init_signal si = 
      match si with
          []              ->   (Stack_const 0,[(0,(false,[],[],[]))])     
        
        | [(id_s,data)]   ->   (Stack_const (id_s+1), append [(id_s,data)] [(id_s+1,(false,[],[],[]))])

        | (id_s,data)::t  ->   let (res,new_si) = init_signal t in (res, append [(id_s,data)] new_si)
    
    
    (* Émet un signal *)
    let rec emit_signal si s = 
      match si with 
          [] -> raise SignalNotInit

        | (id_s,(emit,cs,ssi,tl))::t -> if (id_s = s) then if emit then raise SignalAlreadyEmit else (tl,append [(id_s,(true,cs,ssi,[]))] t) 
                                                      else let (st,new_si) = emit_signal t s in (st,append [(id_s,(emit,cs,ssi,tl))] new_si)


    (* Ajoute une constante dans un signal *)
    let rec put si signal b id = 
      let rec add cs =
        match cs with
            []               ->   [(id,[b])]
          
          | (id1,values)::t  ->   if (id = id1) then (id1,(append values [b]))::t else (id1,values)::(add t) 
      in
      let emit data =
        match data with
            (false,cs,ssi,tl)   ->   (tl,(true,(add cs),ssi,[]))
        
          | (true,cs,ssi,[])    ->   ([],(true,(add cs),ssi,[]))

          | _                   ->   raise UnknowStuckState
      in
      match si with
          []                 ->   raise UnknowSignalState

        | (id_s,data)::t     ->   if (id_s = signal) then let (tl,new_data) = emit data in (tl, append [(id_s,new_data)] t) 
                                                     else let (tl,new_si) = put t signal b id in (tl,append [(id_s,data)] new_si) 


    (* Vérifie si on prend pour la première fois *)
    let rec first_get ci my_id = 
      match ci with 
          [] -> true
          
        | (_,id_list)::t -> if (mem my_id id_list) then false else (first_get t my_id)


    (* Prend une valeurs dans la liste des valeurs partagées d'un signal *)
    let rec get si id_thread neutral signal my_id = 
      let rec remove id_list =
        match id_list with
            []    ->    raise UnknowSignalState

          | h::t  ->   if (my_id = h) then t else append [h] (remove t)
      in
      let rec aux2 ci =
        if( first_get ci my_id)
        then 
          match ci with
              []                                ->   (Stack_const neutral,[],false) 
            
            | [(value,[])]                      ->   (Stack_const value,[(value,[])],true)

            | (value,[])::(value1,id_list1)::t  ->   (Stack_const value,append [(value,[]);(value1,append [my_id] id_list1)] t,false)

            | _                                 ->   raise UnknowSignalState
        else
          match ci with
              []                                     ->   raise UnknowSignalState

            | [(value,id_list)]                      ->   if (mem my_id id_list) 
                                                            then (Stack_const value,[(value,remove id_list)],true) 
                                                            else raise UnknowSignalState

            | (value,id_list)::(value1,id_list1)::t  ->   if (mem my_id id_list) 
                                                            then (Stack_const value,append [(value,remove id_list);(value1,append [my_id] id_list1)] t,false)
                                                            else let (res,new_ci,isEnd) = aux2 (append [(value1,id_list1)] t) in (res,append [(value,remove id_list)] new_ci,isEnd)
      in
      let rec aux1 ssi =
        match ssi with
            []                    ->   raise ThreadSharedNotFound

          | (id1,ci,end_list)::t  ->   if (id1 = id_thread) 
                                        then if (mem my_id end_list)
                                                then (Stack_const neutral,append [(id1,ci,end_list)] t)
                                                else let (res,new_ci,isEnd) = aux2 ci in if isEnd 
                                                                                            then (res,append [id1,new_ci,append [my_id] end_list] t)
                                                                                            else (res,append [id1,new_ci,end_list] t)
                                        else let (res,new_ssi) = aux1 t in (res,append [(id1,ci,end_list)] new_ssi)
      in
      let aux data =
        match data with
          (emit,cs,ssi,tl)        ->   let (res,new_ssi) = aux1 ssi in (res,(emit,cs,new_ssi,tl))
      in
      match si with
          []              ->   raise UnknowSignalState

        | (id_s,data)::t  ->   if (signal = id_s) 
                                then let (res,new_data) = aux data in (res,append [(id_s,new_data)] t) 
                                else let (res,new_si) = get t id_thread neutral signal my_id in (res,append [(id_s,data)] new_si)


    (* Vérifie si la liste de thread bloqué de chaque signal est vide *)
    let rec isEnd si = 
      match si with
          []                            ->   true

        | (signal,(emit,cs,ssi,[]))::t  ->   isEnd t

        | _                             ->   false 


    (* Ajoute un thread dans la liste des threads bloqués d'un signal *)
    let rec add_stuck si signal st =
      let aux data =
        match data with
          (emit,cs,ssi,tl) -> (emit,cs,ssi,(append tl [st]))
      in
      match si with 
          [] -> raise UnknowSignalState

        | (id_s,data)::t -> if (id_s = signal) then (append [(id_s,aux data)] t) else (append [(id_s,data)] (add_stuck t signal st))
    

    (* Applique le second choix sur un thread bloqué *)
    let rec other_choice tl =
      match tl with
          [] -> []

        | Thread(id,Closure([Pair(x2,c2)],e2)::Closure([Pair(x1,c1)],e1)::Stack_const signal::s,e,Present::c,d)::t -> append [Thread(id,s,e,append c2 c,d)] (other_choice t)

        | _ -> raise UnknowStuckState
    
    
    (* Applique tout les changements nécessaires pour changer d'instant logique *)
    let rec next_moment signals =
      let rec aux1 cs =
        match cs with
            [] -> []

          | (id,values)::t -> append [(id,(map (fun x -> (x,[])) values),[])] (aux1 t)
      in
      let rec aux si =
        match si with
            (false,_,_,tl) -> ((other_choice tl) , (false,[],[],[]))

          | (true,cs,_,tl) -> ((other_choice tl) , (false,[],(aux1 cs),[]))
      in
      match signals with
          [] -> ([],[])

        | (si,data)::t -> let (tl,new_signals) = next_moment t in let (tl1,new_data) = aux data in (append tl1 tl , append [(si,new_data)] new_signals)


    let compute stack env op =
      match (stack,op) with 
          (Stack_const b::s,Add1)                  ->   Stack_const(b+1)::s

        | (Stack_const b::s,Sub1)                  ->   Stack_const(b-1)::s

        | (Stack_const 0::s,IsZero)                ->   Closure([Pair("x",[Pair("y",[Variable "x"])])],env)::s

        | (Stack_const b::s,IsZero)                ->   Closure([Pair("x",[Pair("y",[Variable "y"])])],env)::s

        | (Stack_const b::Stack_const b1::s,Add)   ->   Stack_const(b1+b)::s

        | (Stack_const b::Stack_const b1::s,Sub)   ->   Stack_const(b1-b)::s

        | (Stack_const b::Stack_const b1::s,Mult)  ->   Stack_const(b1*b)::s

        | (Stack_const 0::Stack_const b1::s,Div)   ->   raise DivZero

        | (Stack_const b::Stack_const b1::s,Div)   ->   Stack_const(b1/b)::s
        
        | (Stack_const b::s,_)                     ->   raise InsufficientOperandNb

        | (_,_)                                    ->   raise NotAllConstants       





        

    (**** Machine TTSI ****)

    let transition machine =
      match machine with

          (* Constante *)
        | MachineTTSI(Thread(id,s,e,Constant b::c,d),tl,si,ip)                    ->    MachineTTSI( Thread( id , Stack_const b::s , e , c , d ) , tl , si , ip )


          (* Substitution *)
        | MachineTTSI(Thread(id,s,e,Variable x::c,d),tl,si,ip)                    ->   MachineTTSI( Thread( id , (substitution x e)::s , e , c , d ) , tl , si , ip )


          (* Opération *)
        | MachineTTSI(Thread(id,s,e,Prim op::c,d),tl,si,ip)                       ->   MachineTTSI( Thread( id , (compute s e op) , e , c , d ) , tl , si , ip )                        


          (* Abstraction *)
        | MachineTTSI(Thread(id,s,e,Pair(abs,c1)::c,d),tl,si,ip)                  ->    MachineTTSI( Thread( id, Closure([Pair(abs,c1)],e)::s , e , c , d ) , tl , si , ip )
        

          (* Application *)
        | MachineTTSI(Thread(id,v::Closure([Pair(abs,c1)],e1)::s,e,Ap::c,d),tl,si,ip)   
          ->    MachineTTSI( Thread( id , [] , (add_env e1 abs v false) , c1 , Save(s,e,c,d) ) , tl , si , ip )


          (* Récupération d'une sauvegarde *)
        | MachineTTSI(Thread(id,v::s,e,[],Save(s1,e1,c,d)),tl,si,ip)              ->    MachineTTSI( Thread( id , v::s1 , e1 , c , d ) , tl , si , ip )
        

          (* Récupération d'une sauvegarde neutre *)
        | MachineTTSI(Thread(id,s,e,[],Save(s1,e1,c,d)),tl,si,ip)                 ->    MachineTTSI( Thread( id , s1 , e1 , c , d ) , tl , si , ip )


          (* Création d'un thread *)
        | MachineTTSI(Thread(id,Closure([Pair(_,c1)],_)::s,e,Spawn::c,d),tl,si,ip)
          ->    MachineTTSI( Thread( id , Stack_const ip::s , e , c , d ) , (append tl [Thread(ip,[],e,c1,Empty)]) , si , (ip+1) )
              

          (* Ajout d'une valeur *)
        | MachineTTSI(Thread(id,Stack_const signal::Stack_const b::s,e,Put::c,d),tl,si,ip)   
          ->    let (st,new_si) = put si signal b id in MachineTTSI( Thread( id , s , e , c , d ) , (append tl st) , new_si , ip )


          (* Prise d'une valeur *)
        | MachineTTSI(Thread(id,Stack_const n::Stack_const b::Stack_const signal::s,e,Get::c,d),tl,si,ip)          
          ->    let (res,new_si) = get si b n signal id in MachineTTSI( Thread(id , res::s , e , c , d ) , tl , new_si , ip )
                                          

          (* Initialisation d'un signal *)
        | MachineTTSI(Thread(id,s,e,Init::c,d),tl,si,ip)                   
          ->    let (signal,new_si) = init_signal si in MachineTTSI( Thread( id , signal::s , e , c , d ) , tl , new_si , ip )

        
         (* Émission d'un signal *)
        | MachineTTSI(Thread(id,Stack_const signal::s,e,Emit::c,d),tl,si,ip)                   
          ->    let (st,new_si) = emit_signal si signal in MachineTTSI( Thread( id , s , e , c , d ) , append tl st , new_si , ip )

        
          (* Récursion *)
        | MachineTTSI(Thread(id,Closure([Pair(f,[Pair(x,t)])],e1)::s,e,Fix::c,d),tl,si,ip) 
          -> MachineTTSI(Thread( id , Closure([Pair(x,t)],(add_env e1 f (Closure([Pair(x,t)],e1)) true))::s , e , c , d ) , tl , si , ip )

          
          (* Test de présence *)
        | MachineTTSI(Thread(id,Closure([Pair(x2,c2)],e2)::Closure([Pair(x1,c1)],e1)::Stack_const signal::s,e,Present::c,d),tl,si,ip)               
          ->    if (isEmit si signal)
                  then MachineTTSI( Thread( id , s , e , (append c1 c) , d ) , tl , si , ip )
                  else let st = Thread(id,Closure([Pair(x2,c2)],e2)::Closure([Pair(x1,c1)],e1)::Stack_const signal::s,e,Present::c,d) in
                      begin 
                        match tl with
                            []                            ->   MachineTTSI( Thread( ip , [] , [] , [] , Empty ) , [] , (add_stuck si signal st) , ip+1 )

                          | Thread(id1,s1,e1,c3,d1)::tl1  ->   MachineTTSI( Thread( id1 , s1 , e1 , c3 , d1 ) , tl1 , (add_stuck si signal st) , ip )
                      end


          (* Récupération dans la file d'attente *)                         
        | MachineTTSI(Thread(id,s,e,[],Empty),Thread(id1,s1,e1,c,d)::tl,si,ip)    ->    MachineTTSI( Thread( id1 , s1 , e1 , c , d ) , tl , si , ip )


          (* Fin d'un instant logique ou fin de fonctionnement de la machine *)
        | MachineTTSI(Thread(id,s,e,[],Empty),[],si,ip)                           
          ->   if (isEnd si)
                  then match s with
            
                        | Stack_const b::s1             ->   MachineTTSI( Thread( id , [Stack_const b] , [] , [] , Empty ) , [] , [] , ip )
            
                        | Closure([Pair(x,c)],env)::s1  ->   MachineTTSI( Thread( id , [Closure([Pair(x,c)],env)] , [] , [] , Empty ) , [] , [] , ip )

                        | []                            ->   MachineTTSI( Thread( id , [] , [] , [] , Empty ) , [] , [] , ip )
            
                        | _                             ->   raise UnknowStackState

                  else  let (tl,new_si) = next_moment si in MachineTTSI( Thread( id , s , e , [] , Empty ) , tl , new_si , ip )


          (* Application neutre *)  
        | MachineTTSI(Thread(id,s,e,Ap::c,d),tl,si,ip)                            ->    MachineTTSI( Thread( id , s , e , c , d ) , tl , si , ip )
        

          (* Je ne connais pas cette état ... *)
        | _                                                                       ->    raise StrangeEnd


    (* Applique les règles de la machine TTSI en affichant ou non les étapes *)
    let rec machineTTSI machine afficher =
      match machine with
          MachineTTSI(Thread(id,resultat,[],[],Empty),[],[],ip)  ->   printf "Le résultat est %s \n" (string_of_stack resultat)

        | machine                                                ->   if afficher then afficherTTSI machine else printf ""; machineTTSI (transition machine) afficher 
      
  

    (* Lance et affiche le résultat de l'expression *)
    let startTTSIv3 expression afficher = machineTTSI (MachineTTSI(Thread(0,[],[EnvVar("main",[Constant 0])],(secdLanguage_of_exprISWIM expression),Empty),[],[(-1,(false,[],[],[]))],1)) afficher
    
  end