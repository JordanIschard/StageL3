open String ;;
open Printf ;;
open List ;;
open LangISWIMTTS.ISWIM ;;


module MachineTTS =
  struct

    (**** Types ****)

    (* Petits types très pratique pour ne pas se mélanger dans la compréhension des types suivants *)
    type id_signal  =   int
    type variable   =   string
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
      | Emit                                                    (* émet                           *)
      | InitSignal                                              (* initialise un signal           *)
      | Present                                                 (* test de présence d'un signal   *)
      
     

    (* Ce type représente la chaîne de contrôle de la machineTTS SECD, c'est notre entrée *)
    type control_string = c list   


    

    (**** Environment ****)

    (* type intermédiaire qui va servir à représenter l'environnement *)
    type e =   
        EnvClos of (variable * (control_string * e list))     (* (X,(C,Env))            *)
      | EnvVar  of (variable * control_string)                (* (X,V) V une constante  *)

    (* Ce type représente l'environnement de la machineTTS SECD, c'est notre liste de substitution *)
    type environment = e list




    (**** Stack ****)

    (* type intermédiaire contenant une fermeture qui lie une abstraction à un environnement ou une constante ou encore un signal *)
    type s =  
        Stack_const of int                                    (* constante            *)
      | Closure of (control_string * environment)             (* fermeture (C,Env)    *)

    
    (* Ce type représente la pile de la machineTTS SECD, c'est la où la machineTTS travaille *)
    type stack = s list




    (**** Dump ****)

    (* Ce type représente le dépôt de la machineTTS SECD, c'est-à-dire, l'endroit où l'on sauvegarde l'état 
       de la machineTTS SECD pour travailler sur une autre partie de la chaîne de contrôle *)
    type dump =
        Empty                                                        (* le dépôt est vide    *)
      | Save of stack * environment * control_string * dump          (* (s,e,c,d)            *)




    (**** Thread list ****)

    (* type intermédiaire représentant un thread, comprenant son identifiant, sa pile , son environnement, sa chaîne de contrôle et son dépôt *)
    type thread = Thread of stack * environment * control_string * dump

    (* Ce type représente la file d'attente de la machineTTS , c'est-à-dire, 
       la file contenant les threads qui doivent être traité dans l'instant courant *)
    type thread_list = thread list




    (**** Signals ****)
   
    (* type intermédiaire représentant toute les informations liées à un signal *)
    type data = bool * thread_list
   
    (* type intermédiaire représentant un signal *)
    type si = id_signal * data
   
    (* Ce type représente la liste des signaux de la machineTTS *)
    type signals = si list
   


    

    (**** MachineTTS ****)

    (* Ce type représente la structure de la machineTTS *)
    type ttsi = MachineTTS of thread * thread_list * signals             










    (**** Exception ****)

    exception NoSubPossible                    (* Aucune substitution possible pour cette variable dans l'environnement             *)
    exception StrangeEnd                       (* Même moi je ne sais pas ce qu'il sait passé ...                                   *)

    exception NotAllConstants                  (* Tous les éléments de la pile prisent pour l'opérateurs ne sont pas des constantes *)
    exception InsufficientOperandNb            (* Le nombre d'opérande est insuffisante par rapport au nombre requis                *)
    exception DivZero


    exception UnknowStackState                 (* Le format de la pile est invalide et/ou inconnu                                   *)
    exception UnknowEnvState                   (* Le format de l'environnement est invalide et/ou inconnu                           *)
    exception UnknowStuckState                 (* Le format de la liste d'élément bloqués est invalide et/ou inconnu                *)
    exception UnknowSignalState                   (* Le format de la liste de signaux partagés est invalide                         *)





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
          Const const                           ->   [Constant const]
          
        | Var var                               ->   [Variable var]
            
        | App(expr1,expr2)                      ->   append (append (secdLanguage_of_exprISWIM expr1) (secdLanguage_of_exprISWIM expr2)) [Ap]
            
        | Op(op,liste_expr)                     ->   append (flatten(map secdLanguage_of_exprISWIM liste_expr)) [(Prim(op))]
            
        | Abs(abs,expr)                         ->   [Pair(abs,(secdLanguage_of_exprISWIM expr))]

        | Spawn_ISWIM expr                      ->   [Pair("",(secdLanguage_of_exprISWIM expr)) ; Spawn]

        | Present_ISWIM (signal,expr1,expr2)    ->   [Variable signal ; Pair("",(secdLanguage_of_exprISWIM expr1)) ; Pair("",(secdLanguage_of_exprISWIM expr2)) ; Present]

        | Emit_ISWIM signal                     ->   [Variable signal ; Emit]

        | Signal_ISWIM                          ->   [InitSignal]


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

        | Emit::t                            ->   "emit "^(string_of_control_string t)

        | InitSignal::t                      ->   "init "^(string_of_control_string t)


    (* Convertit un environnement en chaîne de caractères *)
    let rec string_of_environment environment =
      match environment with
          []                                      ->   ""

        | [(EnvClos(var,(control_string,env)))]   ->   "["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_environment env)^"]]"

        | [(EnvVar(var,control_string))]          ->   "["^var^" , "^(string_of_control_string control_string) ^"]"

        | (EnvClos(var,(control_string,env)))::t  ->   "["^var^" , ["^(string_of_control_string control_string) ^" , "^(string_of_environment env)^"]] , "^(string_of_environment t)

        | (EnvVar(var,control_string))::t         ->   "["^var^" , "^(string_of_control_string control_string) ^"] , "^(string_of_environment t)


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
        Thread(stack,env,control_string,dump)  ->    
                                        "\n     STACK   : "^(string_of_stack stack)
                                       ^"\n     ENV     : "^(string_of_environment env)
                                       ^"\n     CONTROL : "^(string_of_control_string control_string)
                                       ^"\n     DUMP    : "^(string_of_dump dump)
                                      

    (* Convertit la liste des threads en chaîne de caractères *)
    let rec string_of_thread_list thread_list =
      match thread_list with
          []         ->   ""

        | [Thread(stack,env,control_string,dump)]    ->    
                                                       "\n   [ STACK   : "^(string_of_stack stack)
                                                      ^"\n     ENV     : "^(string_of_environment env)
                                                      ^"\n     CONTROL : "^(string_of_control_string control_string)
                                                      ^"\n     DUMP    : "^(string_of_dump dump)^"]\n"

        | Thread(stack,env,control_string,dump) ::t  ->    " 
                                                        \n   [ STACK   : "^(string_of_stack stack)
                                                      ^"\n     ENV     : "^(string_of_environment env)
                                                      ^"\n     CONTROL : "^(string_of_control_string control_string)
                                                      ^"\n     DUMP    : "^(string_of_dump dump)^"]"
                                                      ^(string_of_thread_list t)


    (* Convertit toutes les informations sur un signal en chaîne de caractères *)
    let string_of_data data =
      match data with
        (emit,tl)  ->  (string_of_bool emit)^" , "^(string_of_thread_list tl) 
    

    (* Convertit un signal en chaîne de caractères *)
    let string_of_si si =
      match si with
        (id_signal,data)  ->  "     ("^(string_of_int id_signal)^" : "^(string_of_data data)^")"


    (* Convertit la liste de tous les signaux en chaîne de caractères *)
    let rec string_of_signals signals =
      match signals with
          []     ->   ""      

        | si::t  ->   "\n"^(string_of_si si)^(string_of_signals t)


    (* Convertit une machine TTS en chaîne de caractères *)
    let rec string_of_machineTTS machineTTS =
      match machineTTS with
        MachineTTS(thread,thread_list,signals)  ->    
                                        "\n   THREAD  : "^(string_of_thread thread)
                                       ^"\n   THREADS : "^(string_of_thread_list thread_list)
                                       ^"\n   SIGNALS : "^(string_of_signals signals)
                                       ^"\n"


    (* Affiche la machine TTS *)
    let afficherTTS machineTTS = printf "MachineTTS : %s\n" (string_of_machineTTS machineTTS)










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

                                                      
    (* Ajoute une  fermeture à l'environnement *)
    let rec add_env env varToRep stack_element =
      match stack_element with
          Stack_const(b)                ->    begin
                                                match env with
                                                    [] -> [EnvVar(varToRep,[Constant b])]

                                                  | EnvClos(var1,closure)::t -> if (equal var1 varToRep) then append [EnvVar(varToRep,[Constant b])] t 
                                                                                                         else append [EnvClos(var1,closure)] (add_env t varToRep stack_element)

                                                  | EnvVar(var1,control_string)::t -> if (equal var1 varToRep) then append [EnvVar(varToRep,[Constant b])] t 
                                                                                                               else append [EnvVar(var1,control_string)] (add_env t varToRep stack_element)
                                              end

      | Closure(control_string,env1)  ->   begin
                                              match env with
                                                    [] -> [EnvClos(varToRep,(control_string,env1))]

                                                  | EnvClos(var1,closure)::t -> if (equal var1 varToRep) then append [EnvClos(varToRep,(control_string,env1))] t 
                                                                                                         else append [EnvClos(var1,closure)] (add_env t varToRep stack_element)

                                                  | EnvVar(var1,control_string1)::t -> if (equal var1 varToRep) then append [EnvClos(varToRep,(control_string,env1))] t 
                                                                                                                else append [EnvVar(var1,control_string)] (add_env t varToRep stack_element)
                                              end

                
    (* Vérifie si le signal est émit *)
    let rec isEmit si signal = 
      let aux data =
        match data with
            (true,_)      ->   true

          | _             ->   false
      in
      match si with
          []              ->   false

        | (id_s,data)::t  ->   if (id_s = signal) then (aux data) else (isEmit t signal)

    
    let rec emit_signal si signal =
      let aux data =
        match data with
            (_,st) -> (st,(true,[]))
      in
      match si with
          [] -> raise UnknowSignalState

        | (id_s,data)::t  -> if (id_s = signal) then let (st,new_data) = aux data             in (st,append [(id_s,new_data)] t)
                                                else let (st,new_si)   = emit_signal t signal in (st,append [(id_s,data)] new_si)

    (* Initialise un signal *)
    let rec init_signal si = 
      match si with
          []              ->   (Stack_const 0,[(0,(false,[]))])     
        
        | [(id_s,data)]   ->   (Stack_const (id_s+1), append [(id_s,data)] [(id_s+1,(false,[]))])

        | (id_s,data)::t  ->   let (res,new_si) = init_signal t in (res, append [(id_s,data)] new_si)


    (* Vérifie si la liste de thread bloqué de chaque signal est vide *)
    let rec isEnd si = 
      match si with
          []                            ->   true

        | (signal,(emit,[]))::t         ->   isEnd t

        | _                             ->   false 


    (* Ajoute un thread dans la liste des threads bloqués d'un signal *)
    let rec add_stuck si signal st =
      let aux data =
        match data with
          (emit,tl) -> (emit,(append tl [st]))
      in
      match si with 
          []             -> raise UnknowSignalState

        | (id_s,data)::t -> if (id_s = signal) then (append [(id_s,aux data)] t) else (append [(id_s,data)] (add_stuck t signal st))
    

    (* Applique le second choix sur un thread bloqué *)
    let rec other_choice tl =
      match tl with
          [] -> []

        | Thread(Closure([Pair(x2,c2)],e2)::Closure([Pair(x1,c1)],e1)::Stack_const signal::s,e,Present::c,d)::t -> append [Thread(s,e,append c2 c,d)] (other_choice t)

        | _ -> raise UnknowStuckState
    
    
    (* Applique tout les changements nécessaires pour changer d'instant logique *)
    let rec next_moment signals =
      let rec aux data =
        match data with
            (_,tl) -> ((other_choice tl) , (false,[]))
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





        

    (**** Machine TTS ****)

    let compute machine =
      match machine with
          (* On a une constante dans la chaîne de contrôle, on la place dans la pile *)
        | MachineTTS(Thread(s,e,Constant b::c,d),tl,si)                    ->    MachineTTS( Thread( Stack_const b::s , e , c , d ) , tl , si )


          (* On a une variable dans la chaîne de contrôle, on place sa substitution (stockée dans l'environnement) dans la pile *)
        | MachineTTS(Thread(s,e,Variable x::c,d),tl,si)                    ->   MachineTTS( Thread( (substitution x e)::s , e , c , d ) , tl , si )


          (* On a prim dans la chaîne de contrôle, on prends le nombre d'élément nécessaire au bon fonctionnement de l'opérateur lié à prim dans la pile 
            et on effectue le calcul. On mets le résultat dans la pile *)
        | MachineTTS(Thread(s,e,Prim op::c,d),tl,si)                       ->   MachineTTS( Thread( (compute s e op) , e , c , d ) , tl , si )                        


          (* On a une abstraction dans la chaîne de contrôle, on place une fermeture ,qui comporte l'abstraction et l'environnment courant, dans la pile *)
        | MachineTTS(Thread(s,e,Pair(abs,c1)::c,d),tl,si)                  ->    MachineTTS( Thread( Closure([Pair(abs,c1)],e)::s , e , c , d ) , tl , si )
        

          (* On a Ap dans la chaîne de contrôle, on sauvegarde une partie de la machine TTS dans le dépôt, on prends l'environnement de la fermeture et on ajoute la nouvelle substitution *)
        | MachineTTS(Thread(v::Closure([Pair(abs,c1)],e1)::s,e,Ap::c,d),tl,si)   
          ->    MachineTTS( Thread(  [] , (add_env e1 abs v) , c1 , Save(s,e,c,d) ) , tl , si )


          (* On a la chaîne de contrôle vide et le dépôt à une sauvegarde, on prends la sauvegarde et on l'applique sur la machineTTS *)
        | MachineTTS(Thread(v::s,e,[],Save(s1,e1,c,d)),tl,si)              ->    MachineTTS( Thread( v::s1 , e1 , c , d ) , tl , si )
        
        (* On a la chaîne de contrôle vide et le dépôt à une sauvegarde, on prends la sauvegarde et on l'applique sur la machineTTS *)
        | MachineTTS(Thread(s,e,[],Save(s1,e1,c,d)),tl,si)                 ->    MachineTTS( Thread( s1 , e1 , c , d ) , tl , si )


          (* On a Bspawn dans la chaîne de contrôle, on prends la partie de la chaîne de contrôle compris entre Bspawn et Espawn et le mets dans un nouveau thread *)
        | MachineTTS(Thread(Closure([Pair(_,c1)],_)::s,e,Spawn::c,d),tl,si)
          ->    MachineTTS( Thread(  s , e , c , d ) , (append tl [Thread(s,e,c1,d)]) , si )
                                          

          (* On a un signal s in t dans la chaîne de contrôle, on remplace la chaîne de contrôle (que l'on stock dans le dépôt) par t et on sauvegarde dans le dépôt le reste plus le signal *)
        | MachineTTS(Thread(s,e,InitSignal::c,d),tl,si)                   
          ->    let (signal,new_si) = init_signal si in MachineTTS( Thread( signal::s , e , c , d ) , tl , new_si )


        | MachineTTS(Thread(Stack_const signal::s,e,Emit::c,d),tl,si)                   
          ->    let (st,new_si) = emit_signal si signal in MachineTTS( Thread( s , e , c , d ) , append tl st , new_si )

          (* On a un present dans la chaîne de contrôle, on regarde si le signal est émit : si oui on prends la première possibilités sinon on le mets dans la liste de threads bloqués *)
        | MachineTTS(Thread(Closure([Pair(x2,c2)],e2)::Closure([Pair(x1,c1)],e1)::Stack_const signal::s,e,Present::c,d),tl,si)               
          ->    if (isEmit si signal)
                  then MachineTTS( Thread( s , e , (append c1 c) , d ) , tl , si )
                  else let st = Thread(Closure([Pair(x2,c2)],e2)::Closure([Pair(x1,c1)],e1)::Stack_const signal::s,e,Present::c,d) in
                      begin 
                        match tl with
                            []                        ->   MachineTTS( Thread( [] , [] , [] , Empty ) , [] , (add_stuck si signal st) )

                          | Thread(s1,e1,c3,d1)::tl1  ->   MachineTTS( Thread( s1 , e1 , c3 , d1 ) , tl1 , (add_stuck si signal st) )
                      end


          (* On a rien dans la chaîne de contrôle et le dépôt est vide mais la liste d'attente à au moins un élément, on prends un thread dans la liste d'attente *)                         
        | MachineTTS(Thread(s,e,[],Empty),Thread(s1,e1,c,d)::tl,si)    ->    MachineTTS( Thread( s1 , e1 , c , d ) , tl , si )


          (* On a rien dans la chaîne de contrôle, le dépôt est vide et la liste d'attente aussi, 
            c'est la fin d'un instant où la fin du fonctionnement de la machine TTS si la liste de threads bloqués est vide *)
        | MachineTTS(Thread(s,e,[],Empty),[],si)                           
          ->   if (isEnd si)
                  then match s with
            
                        | Stack_const b::s1             ->   MachineTTS( Thread( [Stack_const b] , [] , [] , Empty ) , [] , [] )
            
                        | Closure([Pair(x,c)],env)::s1  ->   MachineTTS( Thread( [Closure([Pair(x,c)],env)] , [] , [] , Empty ) , [] , [] )

                        | []                            ->   MachineTTS( Thread( [] , [] , [] , Empty ) , [] , [] )
            
                        | _                             ->   raise UnknowStackState

                  else  let (tl,new_si) = next_moment si in MachineTTS( Thread( s , e , [] , Empty ) , tl , new_si )


          (* On a Ap dans la chaîne de contrôle, on enlève Ap *)  
        | MachineTTS(Thread(s,e,Ap::c,d),tl,si)                            ->    MachineTTS( Thread( s , e , c , d ) , tl , si )
        

          (* Je ne connais pas cette état ... *)
        | _                                                                ->    raise StrangeEnd


    (* Applique les règles de la machine TTS en affichant ou non les étapes *)
    let rec machineTTS machine afficher =
      match machine with
          MachineTTS(Thread(resultat,[],[],Empty),[],[])  ->   printf "Le résultat est %s \n" (string_of_stack resultat)

        | machine                                                ->   if afficher then afficherTTS machine else printf ""; machineTTS (compute machine) afficher 
      
  

    (* Lance et affiche le résultat de l'expression *)
    let startTTS expression afficher = machineTTS (MachineTTS(Thread([],[],(secdLanguage_of_exprISWIM expression),Empty),[],[])) afficher
    
  end