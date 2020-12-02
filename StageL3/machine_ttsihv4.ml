open String ;;
open List ;;
open Lang_ttsi.ISWIM ;;

(****** Ce module implante une 5ème version de la machine abstraite TTSI. Cette version modifie la règle de décomposition. *****)
(****** En effet, on prend en compte que les patterns imbriqués n'existent pas à ce niveau                                 *****)
(****** L'initialisation est limité à un thread                                                                            *****)
module MachineTTSIH =
  struct

    (**** Types ****)

    (* identifiant *)
    type identifier = int 

    (* liste d'identifiants *)
    type identifiers = int list

    (* booléen représentant l'émission d'un signal *)
    type emit = bool 

    (* variable applicable *)
    type variable = string

    (* pattern utilisé pour faire du filtrage *)
    type pattern = 
      | Var of variable                                 (* on filtre avec une variable *)
      | Pat of identifier * pattern list                (* on filtre avec un pattern   *)
      | Neutral

    (* élément accepté par la chaîne de contrôle *)
    type element = 
      | Constant of int                              (* une constante   *)
      | Variable of variable                         (* une variable    *)
      | Abstraction of variable * element list       (* une abstraction *)
      | Pattern of pattern                           (* un pattern      *)
      | Neutral

      | Ap                                           (* commande : appliquer                        *)
      | Prim of operateur                            (* commande : calculer                         *)
      | Spawn                                        (* commande : créer un thread                  *)
      | Present                                      (* commande : tester la présence d'un signal   *)
      | Init                                         (* commande : initialiser un signal            *) 
      | Put                                          (* commande : ajouter une valeur à partager    *)
      | Get                                          (* commande : prendre une valeur partager      *)
      | Fix                                          (* commande : récurrence                       *)
      | Build                                        (* commande : construire un type               *)
      | Compare                                      (* commande : comparer deux types              *)
      | Destruct                                     (* commande : Décompose un type via un pattern *)  
      | Raise                                        (* commande : lève une exception               *)
      | Catch                                        (* commande : gestion d'exception              *)
      
    (* Représentation de la chaîne de contrôle *)
    type control = element list
    
    (* élément accepté dans la pile d'exécution *)
    type value =                                         
      | Const of int                                                            (* une constante *)
      | Closure of ((variable * control) * ( (bool * variable * value) list ))  (* une fermeture *)
      | Type of identifier * value list                                         (* un type       *)
      | P of pattern                                                            (* un pattern    *)
      | Neutral

    (* Représentation de la pile d'exécution *)
    type stack  = value list 

    (* Représentation de l'environnement, liste où l'on stocke les substitution *)
    type environment = (bool * variable * value) list 

    (* Représentation du dépôt, zone où l'on stocke la sauvegarde du thread courant *)
    type dump = 
      | Empty
      | Save of stack * environment * control * dump


    type handler =  
      | Handler of identifier * stack * environment * control * dump * handler
      | None

    (* thread *)
    type thread = Thread of identifier * stack * environment * control * dump * handler

    (* liste de threads *)
    type thread_list = thread list

    (* liste des threads bloqués *)
    type stuck = thread_list

    (* liste des valeurs courantes rangées par identifiant *)
    type current_values = (identifier * int list) list 

    (* liste des valeurs partagées rangées par identifiant *)
    type shared_values = (identifier * (int * identifiers ) list ) list

    (* un signal *)
    type signal = Signal of identifier * ( identifier list * emit * current_values * shared_values * stuck )

    (* liste de signaux *)
    type signal_list = signal list

    (* créateur d'identifiants *)
    type identifier_producer = int 

    (* machine *)
    type machine = Machine of thread * thread_list * signal_list * identifier_producer






    (**** Exception ****)


    exception NoSubPossible                  (* Aucune substitution trouvée dans l'environnement          *)
    exception Strange                        (* Aucune règle ne concorde avec l'état de la machine        *)
    exception NotEnoughElem                  (* Pas assez d'éléments dans la pile d'exécution             *)
    exception InvalidResult                  (* Le résultat de prim est incohérent                        *)
    exception NotAllConstant                 (* Tous les éléments ne sont pas des constantes              *)
    exception SignalNotFound                 (* Le signal voulu n'est pas trouvé dans la liste            *)
    exception InvalidFormatStuck             (* Le format du thread bloqués est invalide                  *)
    exception ThreadNotFound                 (* L'id de thread voulu n'est pas trouvé dans la liste       *)
    exception NoValueToGet                   (* Aucune valeurs n'est partagées dans la liste              *)
    exception IteratorNotFound               (* L'itérateur n'est pas trouvé                              *)
    exception ElemNotFound                   (* L'élément n'est pas trouvé dans la liste                  *)
    exception UnknowStackState               (* La tête de la pile d'exécution n'est pas traitable        *)
    exception FormatRecInvalid               (* La récursion est appliqué sur un format invalide          *)
    exception FormatCreateInvalid            (* Le nombre de paramètres est invalide                      *)
    exception InvalidElemCompared            (* Les éléments comparés ne sont pas deux types              *)
    exception FormatDestructInvalid          (* Le destruct n'est pas appliquée sur un type et un pattern *)
    exception PutInvalid
    exception UnknowSignalState
    exception SignalNotInit




    (**** Convertir ****)

    (* Convertit le langage ISWIM en langage machine *)
    let rec convert_to_machine_language expression = 

      (* Convertit un pattern en langage machine *)
      let rec convert_of_pattern pattern =
        match pattern with
          | Lang_ttsi.ISWIM.Variable variable         ->  Var variable

          | Lang_ttsi.ISWIM.Pattern (id,pat_list)     ->  Pat(id,map convert_of_pattern pat_list)

          | Lang_ttsi.ISWIM.Neutral                   ->  Neutral
      in

      (* Convertit un match en conditionnelle *)
      let rec convert_of_match variable patterns = 
        match patterns with
          | []                 ->  []

          | (pattern,expr)::t  ->  (append (append [Abstraction("v",[Abstraction("t",[Abstraction("f1",[Variable "v";Variable "t";Ap;Variable "f1";Ap])])])] (append [Variable variable] 
                                   [Pattern(convert_of_pattern pattern);Compare])) [Ap;Abstraction("",append [Pattern(convert_of_pattern pattern);Variable variable;Destruct] 
                                   (convert_to_machine_language expr)) ; Ap ; Abstraction("",(convert_of_match variable t)) ; Ap])
      in
      match expression with

          (* X *)                                                   (* [X] *)
        | Lang_ttsi.ISWIM.Var variable                          ->  [Variable variable]

          (* Abs(X,C) *)                                            (* [Abstraction(X,C)] *)
        | Lang_ttsi.ISWIM.Abs (variable,expr)                   ->  [Abstraction (variable,convert_to_machine_language expr)]

          (* App(C,C') *)                                           (* [ C ; C' ; Ap ] *)
        | Lang_ttsi.ISWIM.App (expr1,expr2)                     ->  append (convert_to_machine_language expr1) (append (convert_to_machine_language expr2) [Ap])

          (* Op(op,C) *)                                            (* [ [C] ; Prim op ] *)
        | Lang_ttsi.ISWIM.Op (op,expr_list)                     ->  append (flatten (map convert_to_machine_language expr_list)) [Prim op]

          (* b *)                                                   (* [b] *)
        | Lang_ttsi.ISWIM.Const const                           ->  [Constant const]

          (* Spawn(C) *)                                            (* [Abstraction("",C) ; Spawn ] *)
        | Lang_ttsi.ISWIM.Spawn expr                            ->  [Abstraction ("",convert_to_machine_language expr);Spawn]

          (* Present(X,C,C') *)                                     (* [ X ; Abstraction("",C) ; Abstraction("",C') ; Present ] *)
        | Lang_ttsi.ISWIM.Present (variable,expr1,expr2)        ->  [Variable variable;Abstraction("",convert_to_machine_language expr1);Abstraction("",convert_to_machine_language expr2);Present]

          (* Signal *)                                              (* [Init] *)
        | Lang_ttsi.ISWIM.Signal                                ->  [Init]

          (* Put(X,b) *)                                            (* [ b ; X ; Put ] *)
        | Lang_ttsi.ISWIM.Put (variable,const)                  ->  [Constant const;Variable variable;Put]

          (* Get(X,X',b) *)                                         (* [ b ; X ; X' ; Get ] *)
        | Lang_ttsi.ISWIM.Get (var1,var2,const)                 ->  [Constant const;Variable var1;Variable var2;Get]

          (* Emit(X) *)                                             (* [ Neutral ; X ; Put ] *)
        | Lang_ttsi.ISWIM.Emit (variable)                       ->  [Neutral;Variable variable;Put]

          (* Wait *)                                                (* [ -1 ; Abstraction("",_) ; Abstraction("",_) ; Present ] *)
        | Lang_ttsi.ISWIM.Wait                                  ->  [Constant(-1);Abstraction("",[]);Abstraction("",[]);Present]

          (* Rec(X,C) *)                                            (* [ Abstraction(X,C) ; Fix ] *)
        | Lang_ttsi.ISWIM.Rec (variable,expr)                   ->  [Abstraction(variable,convert_to_machine_language expr);Fix]

          (* if(C,C',C'') *)                                         
        | Lang_ttsi.ISWIM.If (expr1,expr2,expr3)                ->  (append (append [ Abstraction("v",[Abstraction("t",[Abstraction("f1",[Variable "v";Variable "t";Ap;Variable "f1";Ap])])])]   
                                                                    (convert_to_machine_language expr1)) [Ap;Abstraction("",(convert_to_machine_language expr2)) ; Ap ; Abstraction("",(convert_to_machine_language expr3)) ; Ap])

        | Lang_ttsi.ISWIM.Build (id,const,expr_list)            ->  append (flatten (map convert_to_machine_language expr_list)) [Constant const ; Constant id;Build]

        | Lang_ttsi.ISWIM.Match (variable,patterns)             ->  convert_of_match variable patterns

        | Lang_ttsi.ISWIM.Raise variable                        ->  [Variable variable;Raise] 

        | Lang_ttsi.ISWIM.Catch (expr,(variable,expr1))         ->  [Abstraction("",convert_to_machine_language expr);Abstraction(variable,convert_to_machine_language expr1);Catch]






    (**** Affichage ****)


    (* Convertit une liste en chaîne de caractère *)
    let rec string_of_a_list string_of_a l inter =
      match l with
        | []    ->   ""
        | [h]   ->   (string_of_a h)
        | h::t  ->   (string_of_a h)^inter^(string_of_a_list string_of_a t inter)
    

    (* Convertit le pattern en chaîne de caractères *)
    let rec string_of_pattern pattern = 
      match pattern with
        | Var variable                   ->  variable^" "      
        | Pat (identifier,pattern_list)  ->  "["^(string_of_int identifier)^","^(string_of_a_list string_of_pattern pattern_list ",")^"] "
        | Neutral                        ->  "_ "


    (* Convertit un élément de la chaîne de contrôle en chaîne de caractères *)
    let rec string_of_element elem =
      match elem with                           
        | Constant const                       ->   (string_of_int const)
        | Variable variable                    ->   variable
        | Abstraction (variable,control_list)  ->   "<"^variable^","^(string_of_a_list string_of_element control_list " ")^">"
        | Pattern pattern                      ->   (string_of_pattern pattern)
        | Ap                                   ->   "AP" 
        | Prim op                              ->   (string_of_operateur op)
        | Spawn                                ->   "SPAWN"
        | Present                              ->   "PRESENT"
        | Init                                 ->   "INIT" 
        | Put                                  ->   "PUT"        
        | Get                                  ->   "GET"        
        | Fix                                  ->   "FIX"        
        | Build                                ->   "BUILD"        
        | Compare                              ->   "COMPARE"        
        | Destruct                             ->   "DESTRUCT"
        | Neutral                              ->   "neutral"
        | Raise                                ->   "RAISE"
        | Catch                                ->   "CATCH"

    
    (* Convertit la chaîne de contrôle en chaîne de caractères *)
    let string_of_control control = string_of_a_list string_of_element control " "


    (* Convertit l'état d'une substitution, soit normale soit récursive *)
    let string_of_rec recursion = if recursion then "Rec" else ""


    (* Convertit les valeurs en chaîne de caractères *)
    let rec string_of_value value = 

      (* Convertit l'environnement stocké dans une fermeture en chaîne de caractères *)
      let rec aux env =
        match env with
          | (r,variable,value)              ->   (string_of_rec r)^"("^variable^","^(string_of_value value)^")"
      in
      match value with  
        | Const const                       ->   (string_of_int const)^" "

        | Closure ((variable,control),env)  ->   "<<"^variable^","^(string_of_control control)^"> "^(string_of_a_list aux env ",")^"> "

        | Type (identifier,values)          ->   "["^(string_of_int identifier)^","^(string_of_a_list string_of_value values " ")^"] "

        | P pattern                         ->   (string_of_pattern pattern)^" "

        | Neutral                           ->   "neutral "


    (* Convertit la pile d'execution en chaîne de caractères *)
    let string_of_stack stack = string_of_a_list string_of_value stack " "
      
    
    (* Convertit les éléments de l'environnement en chaîne de caractères *)
    let string_of_elem_env elem = 
      match elem with
        | (r,variable,value)   ->   (string_of_rec r)^"("^variable^","^(string_of_value value)^")"


    (* Convertit l'environnement en chaîne de caractères *)
    let string_of_environment environment = string_of_a_list string_of_elem_env environment ","


    (* Convertit le dépôt en chaîne de caractères *)
    let rec string_of_dump dump =
      match dump with
        | Empty            ->   "Vide"

        | Save(s,e,c,d)    ->   "<"^(string_of_stack s)^","^(string_of_environment e)^","^(string_of_control c)^","^(string_of_dump d)^">"


    let rec string_of_handler handler = 
      match handler with
        | None             ->   "Vide"
        
        | Handler(i,s,e,c,d,h) ->    "\n     ID :   "^(string_of_int i)
                                    ^"\n     S  :   "^(string_of_stack s)
                                    ^"\n     E  :   "^(string_of_environment e)
                                    ^"\n     C  :   "^(string_of_control c)
                                    ^"\n     D  :   "^(string_of_dump d) 
                                    ^"\n     H  :   "^(string_of_handler h) 


    (* Convertit le thread en chaîne de caractères *)
    let string_of_thread t = 
      match t with
        | Thread(i,s,e,c,d,h)  ->   "\n  ID :   "^(string_of_int i)
                                   ^"\n  S  :   "^(string_of_stack s)
                                   ^"\n  E  :   "^(string_of_environment e)
                                   ^"\n  C  :   "^(string_of_control c)
                                   ^"\n  D  :   "^(string_of_dump d) 
                                   ^"\n  H  :   "^(string_of_handler h) 
    
    

    (* Convertit la liste de thread en chaîne de caractères *)
    let string_of_thread_list tl = string_of_a_list string_of_thread tl ","


    (* Convertit une valeur courante en chaîne de caractères *)
    let string_of_cv cv =
      match cv with
        | (id,int_list)   ->   "("^(string_of_int id)^","^(string_of_a_list string_of_int int_list ";")^")"


    (* Convertit la liste des valeurs en chaîne de caractères *)
    let string_of_cv_list cv_list = string_of_a_list string_of_cv cv_list " , " 


    (* Convertit une valeur partagée en chaîne de caractères *)
    let string_of_sv sv =
      let aux values =
        match values with
          | (value,identifiers)   ->   "("^(string_of_int value)^",{"^(string_of_a_list string_of_int identifiers ",")^"})"
      in
      match sv with
        | (id,values)             ->   "("^(string_of_int id)^",{"^(string_of_a_list aux values ";")^"})"

    
    (* Convertit la liste des valeurs partagées en chaîne de caractères *)
    let string_of_sv_list sv_list = string_of_a_list string_of_sv sv_list " , "
        

    (* convertit un signal en chaîne de caractères *)
    let string_of_signal si =
      match si with
        | Signal(id,(init_list,emit,cv,sv,stuck))  ->   "<"^(string_of_int id)^",<"^(string_of_a_list string_of_int init_list ";")^","^(string_of_bool emit)^",{"
                                                        ^(string_of_cv_list cv)^"},{"^(string_of_sv_list sv)^"},{"^(string_of_thread_list stuck)^"}>>"


    (* convertit la liste des signaux en chaîne de caractères *)
    let string_of_signals si = string_of_a_list string_of_signal si " , "


    (* convertit la machine en chaîne de caractères *)
    let string_of_machine machine =
      match machine with
        Machine(t,tl,si,ip)  ->     "T  :  "^(string_of_thread t)
                                 ^"\nTL :  ["^(string_of_thread_list tl)^"]"
                                 ^"\nSI :  ["^(string_of_signals si)^"]"
                                 ^"\nIP :  "^(string_of_int ip)

                                 
    (* Affiche la machine *)
    let print_machine machine = Printf.printf "Machine : \n%s\n\n" (string_of_machine machine)






    (**** Fonctions utiles ****)


    (* Ajoute une substitution dans l'environnement *)
    let rec add env x value recursion =
      match env with
        | []                 ->   [(recursion,x,value)]

        | (r,var,value1)::t  ->   if var = x then (recursion,var,value)::t else append [(r,var,value1)] (add t x value recursion)


    (* Substitue une variable par un élément de l'environnement *)
    let rec substitution e x = 
      match e with
        | []                                ->   raise NoSubPossible

        | (true,var,Closure((x1,c),e1))::t  ->   if x = var then Closure((x1,c),(add e1 var (Closure((x1,c),e1)) true)) else substitution t x

        | (false,var,value)::t              ->   if x = var then value else substitution t x

        | _                                 ->   raise FormatRecInvalid


    (* Sépare la pile d'exécution en 2 partie : la 1ère est la partie utilisée pour le calcul *)
    let rec split_for_compute l nbr =
      match (l,nbr) with
        | ([],n)          ->   if n = 0 then ([],[]) else raise NotEnoughElem

        | (h,0)           ->   ([],h)

        | (Const b::t,n)  ->   if n = 0 then ([],Const b::t) else let (elem,stack) = split_for_compute t (n-1) in (append elem [b],stack)

        | (h::_,_)        ->   raise NotAllConstant


    (* Calcul *)
    let compute stack op env = let (operands,stack1) = split_for_compute stack (nbr_operande op) in 
      match (calculer op operands) with 
        | Const b   ->   Const b::stack1

        | Abs(x,c)  ->   Closure((x,convert_to_machine_language c),env)::stack1

        | _         ->   raise InvalidResult



    let rec is_init si i signal = 
      match si with
        | []  -> false
    
        | Signal(id,(init_list,_,_,_,_))::t  ->  if id = signal && mem i init_list then true else is_init t i signal

    let rec first_init si i = 
      match si with
        | []  -> true

        | Signal(_,(init_list,_,_,_,_))::t  ->  if mem i init_list then false else first_init t i
        
        
    (* Initialise un signal *)
    let init si i = 
      let rec aux si = 
        match si with
          | [] ->  (0,[Signal(0,([i],false,[],[],[]))])

          | [Signal(id,(init_list,emit,cv,sv,stuck))]  ->   if mem i init_list then (id+1,[Signal(id,(init_list,emit,cv,sv,stuck));Signal(id+1,([i],false,[],[],[]))])
                                                                               else raise UnknowSignalState

          | Signal(id,(init_list,emit,cv,sv,stuck))::Signal(id1,(init_list1,emit1,cv1,sv1,stuck1))::t 
            -> if mem i init_list 
                  then if mem i init_list1 
                          then  let (res,new_si) = aux (Signal(id1,(init_list1,emit1,cv1,sv1,stuck1))::t ) in (res,Signal(id,(init_list,emit,cv,sv,stuck))::new_si)
                          else  (id1,Signal(id,(init_list,emit,cv,sv,stuck))::Signal(id1,(append init_list1 [i],emit1,cv1,sv1,stuck1))::t)
                  else raise UnknowSignalState
          
      in
      if first_init si i 
        then match si with
                | [] ->  (0,[Signal(0,([i],false,[],[],[]))]) 

                | Signal(id,(init_list,emit,cv,sv,stuck))::t  ->   (id,Signal(id,(append init_list [i],emit,cv,sv,stuck))::t)

        else aux si 


    (* Vérifie si un signal est émis ou non *)
    let rec is_emit si id =
      match si with
        | []                          ->   raise SignalNotFound

        | Signal(id1,(_,e,_,_,_))::t  ->   if id = id1 then e else is_emit t id


    (* Bloque un thread qui attend un signal *)
    let rec stuck thread n si = 
      match si with
        | []                                  ->   raise SignalNotFound

        | Signal(id,(init,emit,cv,sv,st))::t  ->   if id = n then Signal(id,(init,emit,cv,sv,append st [thread]))::t else Signal(id,(init,emit,cv,sv,st))::(stuck thread n t)


    (* Prend le choix du teste de présence indiquant la non émission du signal attendu *)
    let rec snd_choice st =
      match st with
        | []                                                           ->   []

        | Thread(i,Closure((_,c2),e2)::_::_::s,e,Present::c,d,h)::t    ->   Thread(i,[],e2,c2,Save(s,e,c,d),h)::(snd_choice t)

        | _                                                            ->   raise InvalidFormatStuck


    (* Transforme les valeurs courantes en valeurs partagées *)
    let rec shared cv =
      match cv with 
        | []              ->   []

        | (id,values)::t  ->   (id,(map (fun x -> (x,[])) values))::(shared t)


    (* Applique les modification nécessaire pour passer à l'instant suivant *)
    let rec new_instant si =
      match si with
        | []                             ->   ([],[])
        
        | Signal(id,(init,emit,cv,sv,st))::t  ->   let (tl,new_si) = new_instant t in let new_sv = if emit then shared cv else [] in (append (snd_choice st) tl,Signal(id,(init,false,[],new_sv,[]))::new_si) 


    (* Ajoute une valeur dans les informations d'un signal, plus spécifiquement dans la liste de valeurs courantes *)
    let rec put_value si id signal value = 
      let rec put_in_cv cv b =
        match cv with
          | []               ->   [(id,[b])]

          | (id1,values)::t  ->   if id = id1 then (id1,b::values)::t else (id1,values)::(put_in_cv t b)
      in
      match si with
        | []                              ->   raise SignalNotFound

        | Signal(id1,(init,emit,cv,sv,st))::t  ->   if signal = id1 then match value with 
                                                                          | Const b -> let new_cv = put_in_cv cv b in (st,Signal(id1,(init,true,new_cv,sv,[]))::t)

                                                                          | Neutral -> (st,Signal(id1,(init,true,cv,sv,[]))::t)

                                                                          | _       -> raise PutInvalid

                                                                    else let (tl,new_si) = put_value t id signal value in (tl,Signal(id1,(init,emit,cv,sv,st))::new_si)


    (* Vérifie si c'est la première prise de valeur du thread *)
    let rec first_get values id =
      match values with
        | []              ->   true

        | (_,id_list)::t  ->   if mem id id_list then false else first_get t id


    (* Retire un élément d'une liste *)
    let rec remove elem l =
      match l with
        | []    ->   raise ElemNotFound

        | h::t  ->   if elem = h then t else h::(remove elem t)


    (* Prend une valeur dans les informations d'un signal, spécifiquement sa liste de valeurs partagées *)
    let rec get_value si my_id id_thread s neutral =

      (* Prend une valeur dans la liste et déplace l'itérateur *)
      let rec get_in_values values = 
        match values with
          | []                             ->   raise NoValueToGet

          | [(v,id_list)]                  ->   if mem my_id id_list then ([(v,id_list)],neutral) else raise IteratorNotFound

          | (v,id_list)::(v1,id_list1)::t  ->   if mem my_id id_list then ((v,(remove my_id id_list))::(v1,my_id::id_list1)::t,v)
                                                                     else let (new_values,res) = get_in_values ((v1,id_list1)::t) in ((v,id_list)::new_values,res) 
      in
      (* Test si c'est la 1ère fois que l'on prend dans la liste de valeurs. Si oui on prend le 1ère élément sinon on cherche dans la liste *)
      let test values = if (first_get values my_id)
          then match values with
              | []              ->   raise NoValueToGet

              | (v,id_list)::t  ->   ((v,my_id::id_list)::t,v)
          else get_in_values values 
      in
      (* Prend une valeur dans une liste de variable par rapport à un identifiant de thread *)
      let rec get_in_sv sv =
        match sv with 
          | []              ->   raise ThreadNotFound

          | (id,values)::t  ->   if id = id_thread  then let (new_values,res) = test values in ((id,new_values)::t,res) 
                                                    else let (new_sv,res) = get_in_sv t in ((id,values)::new_sv,res)
      in
      match si with
        | []                             ->   raise SignalNotFound

        | Signal(id,(init,emit,cv,sv,st))::t  ->   if id = s then let (new_sv,res) = get_in_sv sv in (Signal(id,(init,emit,cv,new_sv,st))::t,res)
                                                             else let (new_si,res) = get_value t my_id id_thread s neutral in (Signal(id,(init,emit,cv,sv,st))::new_si,res)


    (* Vérifie si une liste est vide *)
    let isEmpty l = 
      match l with
        | [] -> true

        | _  -> false


    (* Compare deux types *)
    let rec compare t1 t2 env =
      match (t1,t2) with
        | (Type(c,v),P(Pat(c1,v1)))     ->   if c = c1 then Closure(("x",[Abstraction("y",[Variable "x";Constant 1;Ap])]),env)
                                                       else Closure(("x",[Abstraction("y",[Variable "y";Constant 1;Ap])]),env)

        | (_,_)                         ->   raise InvalidElemCompared


    (* Décompose un type par rapport à un pattern *)
    let rec destruct values vars env = 
      match (values,vars) with
       | ([],[])                 ->   env

       | (value::t1,Var var::t)  ->   destruct t1 t (add env var value false)

       | (_,_)                   ->   raise FormatDestructInvalid


    (* Créer un type *)
    let rec create s nbr = 
      match (s,nbr) with
        | ([],0)              ->   ([],[])

        | (s,0)               ->   ([],s)
        
        | (Const c::t,nbr)    ->   let (res,new_s) = create t (nbr-1) in (append res [Const c],new_s)

        | (Type(x,c)::t,nbr)  ->   let (res,new_s) = create t (nbr-1) in (append res [Type(x,c)],new_s)

        | _                   ->   raise NotEnoughElem




    (**** Machine ****)

    (* Applique une transition pour un état de la machine *)
    let transition machine =
      match machine with

        (*** Partie de base de la machine SECD ***)

          (* Constante *)
        | Machine(Thread(i,s,e,Constant b::c,d,h),tl,si,ip)                      ->   Machine(Thread(i,Const b::s,e,c,d,h),tl,si,ip) 


          (* Substitution *)
        | Machine(Thread(i,s,e,Variable x::c,d,h),tl,si,ip)                      ->   Machine(Thread(i,(substitution e x)::s,e,c,d,h),tl,si,ip) 


          (* Abstraction *)
        | Machine(Thread(i,s,e,Abstraction(x,c1)::c,d,h),tl,si,ip)               ->   Machine(Thread(i,Closure((x,c1),e)::s,e,c,d,h),tl,si,ip) 


          (* Application *)
        | Machine(Thread(i,v::Closure((x,c1),e1)::s,e,Ap::c,d,h),tl,si,ip)       ->   Machine(Thread(i,[],(add e1 x v false),c1,Save(s,e,c,d),h),tl,si,ip)


          (* Operation *)
        | Machine(Thread(i,s,e,Prim op::c,d,h),tl,si,ip)                         ->   Machine(Thread(i,(compute s op e),e,c,d,h),tl,si,ip)


          (* Récupération de sauvegarde *)
        | Machine(Thread(i,v::s,e,[],Save(s1,e1,c,d),h),tl,si,ip)                ->   Machine(Thread(i,v::s1,e1,c,d,h),tl,si,ip)





        (*** Partie pour la concurrence ***)

          (* Création thread *)
        | Machine(Thread(i,Closure((_,c1),e1)::s,e,Spawn::c,d,h),tl,si,ip)       ->   Machine(Thread(i,Const ip::s,e,c,d,h),append tl [Thread(ip,[],e1,c1,Empty,None)],si,ip+1)


          (* Initialisation d'un signal *)
        | Machine(Thread(i,s,e,Init::c,d,h),tl,si,ip)                            ->   let (id,new_si) = init si i in Machine(Thread(i,Const id::s,e,c,d,h),tl,new_si,ip)


          (* Teste de présence *)
        | Machine(Thread(i,Closure((_,c2),e2)::Closure((_,c1),e1)::Const n::s,e,Present::c,d,h),tl,si,ip)
          -> if is_init si i n
               then if is_emit si n

                          (* Présence d'un signal *)
                      then Machine(Thread(i,[],e1,c1,Save(s,e,c,d),h),tl,si,ip)
            
                      else let new_si = stuck (Thread(i,Closure(("",c2),e2)::Closure(("",c1),e1)::Const n::s,e,Present::c,d,h)) n si in
                      begin
                        match tl with

                            (* Thread bloqué non remplacé *)
                          | []                            ->   Machine(Thread(ip,[],[],[],Empty,None),[],new_si,ip+1)
                            
                            (* Thread bloqué remplacé *)
                          | Thread(i1,s1,e1,c1,d1,h1)::t  ->   Machine(Thread(i1,s1,e1,c1,d1,h1),t,new_si,ip)
                      end
               else raise SignalNotInit

          (* Récupération dans la file d'attente *)
        | Machine(Thread(i,s,e,[],Empty,h),Thread(i1,s1,e1,c,d,h1)::tl,si,ip)    ->   Machine(Thread(i1,s1,e1,c,d,h1),tl,si,ip)


          (* Fin d'un instant logique *)
        | Machine(Thread(i,s,e,[],Empty,h),[],si,ip)                             ->   let (tl,new_si) = new_instant si in if isEmpty tl then  Machine(Thread(i,s,[],[],Empty,h),[],[],ip)
                                                                                                                                        else  Machine(Thread(i,s,e,[],Empty,h),tl,new_si,ip)


          (* Ajoute une valeur dans un signal *)
        | Machine(Thread(i,Const n::value::s,e,Put::c,d,h),tl,si,ip)             
          ->   let (st,new_si) = if is_init si i n then put_value si i n value else raise SignalNotInit in  Machine(Thread(i,s,e,c,d,h),append tl st,new_si,ip)


          (* Prend une valeur dans un signal *)
        | Machine(Thread(i,Const b::Const j::Const n::Closure((x,c1),e1)::s,e,Get::c,d,h),tl,si,ip)
          -> let (new_si,res) = if is_init si i j then get_value si i b j n else raise SignalNotInit in Machine(Thread(i,[],(add e1 x (Const res) false),c1,Save(s,e,c,d),h),tl,new_si,ip)





        (*** Partie pour la récursion ***)
        
          (* Récursion *)
        | Machine(Thread(i,Closure((f,[Abstraction(x,t)]),e1)::s,e,Fix::c,d,h),tl,si,ip) 
          -> Machine(Thread(i,Closure((x,t),(add e1 f (Closure((x,t),e1)) true))::s,e,c,d,h),tl,si,ip)
        




        (*** Partie types ***)
        
          (* Création d'un type *)
        | Machine(Thread(i,Const id::Const nbr::s,e,Build::c,d,h),tl,si,ip)      ->   let (res,new_s) = create s nbr in Machine(Thread(i,Type(id,res)::new_s,e,c,d,h),tl,si,ip)


          (* Comparer deux types *)
        | Machine(Thread(i,t2::t1::s,e,Compare::c,d,h),tl,si,ip)                 ->   let res = compare t1 t2 e in Machine(Thread(i,res::s,e,c,d,h),tl,si,ip)


          (* Décomposition d'un type via un pattern *)
        | Machine(Thread(i,Type(_,values)::P(Pat(_,vars))::s,e,Destruct::c,d,h),tl,si,ip) 
          ->  let new_e = destruct values vars e in Machine(Thread(i,s,new_e,c,d,h),tl,si,ip)


          (* Décomposition d'un type via un neutre *)
        | Machine(Thread(i,Type(_,values)::P Neutral::s,e,Destruct::c,d,h),tl,si,ip) 
          ->  Machine(Thread(i,s,e,c,d,h),tl,si,ip)

          (* Pattern *)
        | Machine(Thread(i,s,e,Pattern p::c,d,h),tl,si,ip)                       ->   Machine(Thread(i,P p::s,e,c,d,h),tl,si,ip)
        




        (*** Partie gestion d'exceptions ***)

          (* Création du gestionnaire d'exception *)
        | Machine(Thread(i,Closure((x,c2),e2)::Closure((x1,c1),e1)::s,e,Catch::c,d,h),tl,si,ip)   
          ->   Machine(Thread(i,[],e1,c1,Save(s,e,c,d),Handler(i,Closure((x,c2),e2)::s,e,Catch::c,d,h)),tl,si,ip)

        
          (* exception levée et traitée *)
        | Machine(Thread(i,Type(id,values)::s,e,Raise::c,d,Handler(i1,Closure((x,c2),e2)::s1,e1,Catch::c1,d1,h)),tl,si,ip)                      
          ->   Machine(Thread(i1,[],(add e2 x (Type(id,values)) false),c2,Save(s1,e1,c1,d1),h),tl,si,ip)


          (* exception levée non traitée *)
        | Machine(Thread(i,Type(id,values)::s,e,Raise::c,d,h),tl,si,ip)          ->   Machine(Thread(i,[Type(id,values)],e,[],Empty,h),[],[],ip)
    




        (*** Partie commune ***)

          (* Application neutre *)
        | Machine(Thread(i,s,e,Ap::c,d,h),tl,si,ip)                              ->   Machine(Thread(i,s,e,c,d,h),tl,si,ip)


          (* Neutre *)
        | Machine(Thread(i,s,e,Neutral::c,d,h),tl,si,ip)                         ->   Machine(Thread(i,Neutral::s,e,c,d,h),tl,si,ip)


          (* Récupération sauvegarde avec pile vide *)
        | Machine(Thread(i,[],e,[],Save(s,e1,c,d),h),tl,si,ip)                   ->   Machine(Thread(i,s,e1,c,d,h),tl,si,ip)


        | _                                                                      ->   raise Strange


    (* Applique les règles de la machine TTSI en affichant ou non les étapes *)
    let rec machine m afficher =
      match m with
        | Machine(Thread(id,resultat,env,[],Empty,h),[],[],ip)  ->   Printf.printf "Le résultat est %s \n" (string_of_stack resultat)

        | ttsih                                                  ->   if afficher then print_machine ttsih else Printf.printf ""; machine (transition ttsih) afficher 
      

    (* Lance et affiche le résultat de l'expression *)
    let startTTSIHv4 expression afficher = machine (Machine(Thread(0,[],[(false,"main",Const 0)],(convert_to_machine_language expression),Empty,None),[],[(Signal(-1,([],false,[],[],[])))],1)) afficher
    

  end