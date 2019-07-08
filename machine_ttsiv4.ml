open String ;;
open Printf ;;
open List ;;
open Lang_ttsiv4.ISWIM ;;


module MachineTTSI =
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
      | Pat of identifier * pattern list                (* on filtre avec un pattern *)

    (* élément accepté par la chaîne de contrôle *)
    type element = 
      | Constant of int                              (* une constante *)
      | Variable of variable                         (* une variable  *)
      | Abstraction of ( variable * element list )   (* une abstraction *)
      | Pattern of pattern
      | Ap                                           (* commande : appliquer *)
      | Prim of operateur                            (* commande : calculer *)
      | Spawn                                        (* commande : créer un thread *)
      | Present                                      (* commande : tester la présence d'un signal *)
      | Init                                         (* commande : initialiser un signal *) 
      | Emit                                         (* commande : émettre un signal *)
      | Put                                          (* commande : ajouter une valeur à partager *)
      | Get                                          (* commande : prendre une valeur partager *)
      | Fix                                          (* commande : récurrence *)
      | Build                                        (* commande : construire un type *)
      | Compare                                      (* commande : comparer deux types *)
      | Destruct                                     (* commande : Décompose un type via un pattern *)  
      
    type control = element list
    
    (* élément accepté dans la pile d'exécution *)
    type value =                                         
      | Const of int                                                            (* une constante *)
      | Closure of ((variable * control) * ( (bool * variable * value) list ))  (* une fermeture *)
      | Type of (identifier * value list)                                       (* un type *)
      | P of pattern                                                            (* un pattern *)

    (* pile d'exécution *)
    type stack  = value list 

    (* environnement, liste où l'on stocke les substitution *)
    type environment = (bool * variable * value) list 

    (* constructeur pour un type *)
    type builder =  Builder of (identifier * int)      (* le second élément est le nombre de paramètre *)
 
    (* liste de constructeurs *)
    type builder_list = builder list 

    (* dépôt, zone où l'on stocke la sauvegarde du thread courant *)
    type dump = 
      | Empty
      | Save of stack * environment * control * dump * builder_list

    (* thread *)
    type thread = Thread of identifier * stack * environment * control * dump * builder_list

    (* liste de threads *)
    type thread_list = thread list

    (* liste des threads bloqués *)
    type stuck = thread_list

    (* liste des valeurs courantes rangées par identifiant *)
    type current_values = (identifier * int list) list 

    (* liste des valeurs partagées rangées par identifiant *)
    type shared_values = (identifier * (int * identifiers ) list ) list

    (* un signal *)
    type signal = Signal of identifier * (emit * current_values * shared_values * stuck )

    (* liste de signaux *)
    type signal_list = signal list

    (* créateur d'identifiants *)
    type identifier_producer = int 

    (* machine *)
    type machine = Machine of thread * thread_list * signal_list * identifier_producer






    (**** Exception ****)


    exception NoSubPossible
    exception Strange
    exception NotEnoughElem
    exception InvalidResult
    exception NotAllConstant
    exception SignalNotFound
    exception InvalidFormatStuck
    exception ThreadNotFound
    exception NoValueToGet
    exception FormatError
    exception ElemNotFound
    exception UnknowStackState
    exception FormatRecInvalid
    exception BuilderNotFound
    exception FormatCreateInvalid
    exception InvalidElemCompared
    exception FormatDestructInvalid





    (**** Affichage ****)

    (* Convertit le langage ISWIM en langage machine *)
    let rec convert_to_machine_language expression = 
      let rec convert_of_pattern pattern =
        match pattern with
          | Pattern_var variable         ->  Var variable

          | Pattern_ISWIM (id,pat_list)  ->  Pat(id,map convert_of_pattern pat_list)
      in
      let rec convert_of_match variable patterns = 
        match patterns with
          | []                 ->  []

          | (pattern,expr)::t  ->  (append 
                                        (append [Abstraction("v",[Abstraction("t",[Abstraction("f1",[Variable "v";Variable "t";Ap;Variable "f1";Ap])])])]   
                                                (append [Variable variable] 
                                                        [Pattern(convert_of_pattern pattern);Compare])) 
                                        [Ap;Abstraction("",append [Variable variable;Pattern(convert_of_pattern pattern);Destruct] (convert_to_machine_language expr)) ; Ap ; Abstraction("",(convert_of_match variable t)) ; Ap])
      in
      match expression with
        | Var_ISWIM variable                    ->  [Variable variable]

        | Abs (variable,expr)                   ->  [Abstraction (variable,convert_to_machine_language expr)]

        | App (expr1,expr2)                     ->  append (convert_to_machine_language expr1) 
                                                    (append (convert_to_machine_language expr2) [Ap])

        | Op (op,expr_list)                     ->  append (flatten (map convert_to_machine_language expr_list)) [Prim op]

        | Const const                           ->  [Constant const]

        | Spawn_ISWIM expr                      ->  [Abstraction ("",convert_to_machine_language expr);Spawn]

        | Present_ISWIM (variable,expr1,expr2)  ->  [Variable variable;Abstraction("",convert_to_machine_language expr1)
                                                     ;Abstraction("",convert_to_machine_language expr2);Present]

        | Signal_ISWIM                          ->  [Init]

        | Put_ISWIM (variable,const)            ->  [Constant const;Variable variable;Put]

        | Get_ISWIM (var1,var2,const)           ->  [Constant const;Variable var1;Variable var2;Get]

        | Emit_ISWIM (variable)                 ->  [Variable variable;Emit]

        | Wait                                  ->  [Constant(-1);Abstraction("",[]);Abstraction("",[]);Present]

        | Rec (variable,expr)                   ->  [Abstraction(variable,convert_to_machine_language expr);Fix]

        | If (expr1,expr2,expr3)                ->  (append (append [ Abstraction("v",[Abstraction("t",[Abstraction("f1",[Variable "v";Variable "t";Ap;Variable "f1";Ap])])])]   
                                                    (convert_to_machine_language expr1)) [Ap;Abstraction("",(convert_to_machine_language expr2)) ; Ap ; Abstraction("",(convert_to_machine_language expr3)) ; Ap])

        | Build_ISWIM const                     ->  [Constant const;Build]

        | Match (variable,patterns)             ->  convert_of_match variable patterns


    (* Convertit une liste en chaîne de caractère *)
    let rec string_of_list l =
      match l with
        | []    ->   ""
        | [h]   ->   h
        | h::t  ->   h^","^(string_of_list t)
        
    
    (* Convertit le pattern en chaîne de caractères *)
    let rec string_of_pattern pattern = 
      match pattern with
        | Var variable                   ->  variable^" "      
        | Pat (identifier,pattern_list)  ->  "["^(string_of_int identifier)^","^(string_of_list (map string_of_pattern pattern_list))^"] "
        

    (* Convertit la chaîne de contrôle en chaîne de caractères *)
    let rec string_of_control control =
      match control with
        | []                                      ->   ""
        | Constant const::t                       ->   (string_of_int const)^" "^(string_of_control t)
        | Variable variable::t                    ->   variable^" "^(string_of_control t)
        | Abstraction (variable,control_list)::t  ->   "<"^variable^","^(string_of_control control_list)^"> "^(string_of_control t)
        | Pattern pattern::t                      ->   (string_of_pattern pattern)^" "^(string_of_control t)
        | Ap::t                                   ->   "AP "^(string_of_control t) 
        | Prim op::t                              ->   (string_of_operateur op)^" "^(string_of_control t)
        | Spawn::t                                ->   "SPAWN "^(string_of_control t)
        | Present::t                              ->   "PRESENT "^(string_of_control t)
        | Init::t                                 ->   "INIT "^(string_of_control t)        
        | Emit::t                                 ->   "EMIT "^(string_of_control t)
        | Put::t                                  ->   "PUT "^(string_of_control t)        
        | Get::t                                  ->   "GET "^(string_of_control t)        
        | Fix::t                                  ->   "FIX "^(string_of_control t)        
        | Build::t                                ->   "BUILD "^(string_of_control t)        
        | Compare::t                              ->   "COMPARE "^(string_of_control t)        
        | Destruct::t                             ->   "DESTRUCT "^(string_of_control t)


    (* Convertit les valeurs en chaîne de caractères *)
    let rec string_of_value value = 
      let rec aux env =
        match env with
          | []                   ->   ""

          | [(r,variable,value)]   ->   if r then "Rec("^variable^","^(string_of_value value)^") "
                                             else "("^variable^","^(string_of_value value)^")"

          | (r,variable,value)::t  ->   if r then "Rec("^variable^","^(string_of_value value)^") "
                                             else "("^variable^","^(string_of_value value)^"),"^(aux t)
      in
      match value with  
        | Const const                       ->   (string_of_int const)^" "

        | Closure ((variable,control),env)  ->   "<<"^variable^","^(string_of_control control)^"> "^(aux env)^"> "

        | Type (identifier,values)          ->   "["^(string_of_int identifier)^","^(string_of_list (map string_of_value values))^"] "

        | P pattern                         ->   (string_of_pattern pattern)^" "


    (* Convertit la pile d'execution en chaîne de caractères *)
    let rec string_of_stack stack = 
      match stack with
        | []        ->   ""

        | value::t  ->   (string_of_value value)^" "^(string_of_stack t)
      
    
    (* Convertit l'environnement en chaîne de caractères *)
    let rec string_of_environment environment = 
      match environment with
        | []                             ->   ""

        | [(recursion,variable,value)]   ->   if recursion then "Rec("^variable^","^(string_of_value value)^")"
                                                           else "("^variable^","^(string_of_value value)^")"

        | (recursion,variable,value)::t  ->   if recursion then "Rec("^variable^","^(string_of_value value)^")"^","^(string_of_environment t)
                                                           else "("^variable^","^(string_of_value value)^")"^","^(string_of_environment t)

    (* convertit la liste des types en chaîne de caractères *)    
    let rec string_of_liste_type l = 
      match l with
        | []                  ->   ""

        | Builder(id,nbr)::t  ->   "<"^(string_of_int id)^","^(string_of_int nbr)^"> "^(string_of_liste_type t) 
 

    (* convertit le dépôt en chaîne de caractères *)
    let rec string_of_dump dump =
      match dump with
        | Empty            ->   "Vide"

        | Save(s,e,c,d,l)  ->   "<"^(string_of_stack s)^","^(string_of_environment e)^","^(string_of_control c)^","^(string_of_dump d)^","^(string_of_liste_type l)^">"


    (* convertit le thread en chaîne de caractères *)
    let rec string_of_thread t = 
      match t with
        | Thread(i,s,e,c,d,l)  ->   "\n  ID :   "^(string_of_int i)
                                   ^"\n  S  :   "^(string_of_stack s)
                                   ^"\n  E  :   "^(string_of_environment e)
                                   ^"\n  C  :   "^(string_of_control c)
                                   ^"\n  D  :   "^(string_of_dump d) 
                                   ^"\n  L  :   "^(string_of_liste_type l) 
    
    

    (* convertit la liste de thread en chaîne de caractères *)
    let rec string_of_thread_list tl =
      match tl with
        | []         ->   ""

        | [thread]   ->   (string_of_thread thread)

        | thread::t  ->   (string_of_thread thread)^","^(string_of_thread_list t)


    (* convertit la liste des valeurs en chaîne de caractères *)
    let rec string_of_cv cv =
      match cv with
        | []                ->   ""

        | [(id,int_list)]   ->   "("^(string_of_int id)^","^(string_of_list (map string_of_int int_list))^")"

        | (id,int_list)::t  ->   "("^(string_of_int id)^","^(string_of_list (map string_of_int int_list))^") , "^(string_of_cv t)


    (* Convertit la liste des valeurs partagées en chaîne de caractères *)
    let rec string_of_sv sv =
      let rec aux values =
        match values with
          | []                      ->   ""

          | [(value,identifiers)]   ->   "("^(string_of_int value)^",{"^(string_of_list (map string_of_int identifiers))^"})"

          | (value,identifiers)::t  ->   "("^(string_of_int value)^",{"^(string_of_list (map string_of_int identifiers))^"}),"^(aux t)
      in
      match sv with
        | []              ->   ""
        
        | (id,values)::t  ->   "("^(string_of_int id)^",{"^(aux values)^"}) "^(string_of_sv t)
        

    (* convertit un signal en chaîne de caractères *)
    let rec string_of_signal si =
      match si with
        | Signal(id,(emit,cv,sv,stuck))  ->   "<"^(string_of_int id)^",<"^(string_of_bool emit)^",{"^(string_of_cv cv)^"},{"^(string_of_sv sv)^"},{"^(string_of_thread_list stuck)^"}>>"


    (* convertit la liste des signaux en chaîne de caractères *)
    let rec string_of_signals si = 
      match si with
        | []         ->   ""

        | [signal]   ->   (string_of_signal signal)

        | signal::t  ->   (string_of_signal signal)^", "^(string_of_signals t)
    

    (* convertit la machine en chaîne de caractères *)
    let string_of_machine machine =
      match machine with
        Machine(t,tl,si,ip)  ->     "T  :  "^(string_of_thread t)
                                 ^"\nTL :  ["^(string_of_thread_list tl)^"]"
                                 ^"\nSI :  ["^(string_of_signals si)^"]"
                                 ^"\nIP :  "^(string_of_int ip)

    (* Affiche la machine *)
    let print_machine machine = printf "Machine : \n%s\n\n" (string_of_machine machine)






    (**** Fonctions utiles ****)


    (* Ajoute une substitution dans l'environnement *)
    let rec add env x value recursion =
      match env with
        | []                 ->   [(recursion,x,value)]

        | (r,var,value1)::t  ->   if (var = x ) then (recursion,var,value)::t else append [(r,var,value1)] (add t x value recursion)


    (* Substitue une variable par un élément de l'environnement *)
    let rec substitution e x = 
      match e with
        | []                        ->   raise NoSubPossible

        | (true,var,Closure((x1,c),e1))::t -> if (x = var) then Closure((x1,c),(add e1 var (Closure((x1,c),e1)) true)) else substitution t x

        | (false,var,value)::t  ->   if (x = var) then value else substitution t x

        | _ -> raise FormatRecInvalid


    (* Sépare la pile d'exécution en 2 partie : la 1ère est la partie utilisée pour le calcul *)
    let rec split_for_compute l nbr =
      match (l,nbr) with
        | ([],n)          ->   if n = 0 then ([],[]) else raise NotEnoughElem

        | (h,0)           ->   ([],h)

        | (Const b::t,n)  ->   if n = 0 then ([],Const b::t) else let (elem,stack) = split_for_compute t (n-1) in (append elem [b],stack)

        | (h::_,_)        ->   printf "%s" (string_of_stack [h]) ;raise NotAllConstant


    (* Calcul *)
    let compute stack op env = let (operands,stack1) = split_for_compute stack (nbr_operande op) in 
      match (calculer op operands) with 
        | Const b   ->   Const b::stack1

        | Abs(x,c)  ->   Closure((x,convert_to_machine_language c),env)::stack1

        | _         ->   raise InvalidResult

    
    (* Initialise un signal *)
    let rec init si = 
      match si with
        | []                  ->   (0,[Signal(0,(false,[],[],[]))])

        | [Signal(id,data)]   ->   (id+1,[Signal(id,data);Signal(id+1,(false,[],[],[]))])  

        | Signal(id,data)::t  ->   let (new_id,new_si) = init t in (new_id,Signal(id,data)::new_si)


    (* Emet un signal *)
    let rec emit si id =
      match si with
        | []                           ->   raise SignalNotFound 

        | Signal(id1,(e,cv,sv,st))::t  ->   if id = id1 then (st,Signal(id1,(true,cv,sv,[]))::t) 
                                                        else let (tl,new_si) = emit t id in (tl,Signal(id1,(true,cv,sv,[]))::new_si)


    (* Vérifie si un signal est émis ou non *)
    let rec is_emit si id =
      match si with
        | []                        ->   raise SignalNotFound

        | Signal(id1,(e,_,_,_))::t  ->   if id = id1 then e else is_emit t id


    (* Bloque un thread qui attend un signal *)
    let rec stuck thread n si = 
      match si with
        | []                             ->   raise SignalNotFound

        | Signal(id,(emit,cv,sv,st))::t  ->   if id = n then Signal(id,(emit,cv,sv,append st [thread]))::t else Signal(id,(emit,cv,sv,st))::(stuck thread n t)


    (* Prend le choix du teste de présence indiquant la non émission du signal attendu *)
    let rec snd_choice st =
      match st with
        | []                                                         ->   []

        | Thread(i,Closure((_,c2),e2)::_::_::s,e,Present::c,d,l)::t  ->   Thread(i,[],e2,c2,Save(s,e,c,d,l),l)::(snd_choice t)

        | _                                                          ->   raise InvalidFormatStuck


    (* Transforme les valeurs courantes en valeurs partagées *)
    let rec shared cv =
      let rec aux values =
        match values with
         | []    ->   []

         | h::t  ->   (h,[])::(aux t)
      in
      match cv with 
        | []              ->   []

        | (id,values)::t  ->   (id,(aux values))::(shared t)


    (* Applique les modification nécessaire pour passer à l'instant suivant *)
    let rec new_instant si =
      match si with
        | []                             ->   ([],[])
        
        | Signal(id,(emit,cv,sv,st))::t  ->   let (tl,new_si) = new_instant t in let new_sv = if emit then (shared cv) else [] in
                                              (append (snd_choice st) tl,Signal(id,(false,[],new_sv,[]))::new_si) 


    (* Ajoute une valeur dans les informations d'un signal, plus spécifiquement dans la liste de valeurs courantes *)
    let rec put_value si id signal value = 
      let rec put_in_cv cv =
        match cv with
          | []               ->   [(id,[value])]

          | (id1,values)::t  ->   if id = id1 then (id1,value::values)::t else (id1,values)::(put_in_cv t)
      in
      match si with
        | []                              ->   raise SignalNotFound

        | Signal(id1,(emit,cv,sv,st))::t  ->   if signal = id1 then let new_cv = put_in_cv cv in (st,Signal(id1,(true,new_cv,sv,[]))::t)
                                                               else let (tl,new_si) = put_value t id signal value in (tl,Signal(id1,(emit,cv,sv,st))::new_si)


    (* Vérifie si c'est la première prise de valeur du thread *)
    let rec first_get values id =
      match values with
        | [] -> true

        | (_,id_list)::t -> if mem id id_list then false else first_get t id


    (* Retire un élément d'une liste *)
    let rec remove elem l =
      match l with
        | [] -> raise ElemNotFound

        | h::t -> if elem = h then t else h::(remove elem t)


    (* Prend une valeur dans les informations d'un signal, spécifiquement sa liste de valeurs partagées *)
    let rec get_value si my_id id_thread s neutral =
      let rec get_in_values values = 
        match values with
          | [] -> raise NoValueToGet

          | [(v,id_list)] -> if mem my_id id_list then ([(v,id_list)],neutral)
                                                  else raise FormatError

          | (v,id_list)::(v1,id_list1)::t -> if (mem my_id id_list) then ((v,(remove my_id id_list))::(v1,my_id::id_list1)::t,v)
                                                                    else let (new_values,res) = get_in_values ((v1,id_list1)::t) in
                                                                          ((v,id_list)::new_values,res) 
      in
      let test values = if (first_get values my_id)
          then match values with
              | [] -> raise NoValueToGet

              | (v,id_list)::t -> ((v,my_id::id_list)::t,v)
          else get_in_values values 
      in
      let rec get_in_sv sv =
        match sv with 
          | [] -> raise ThreadNotFound

          | (id,values)::t -> if id = id_thread  then let (new_values,res) = test values in ((id,new_values)::t,res) 
                                                 else let (new_sv,res) = get_in_sv t in ((id,values)::new_sv,res)
      in
      match si with
        | [] -> raise SignalNotFound

        | Signal(id,(emit,cv,sv,st))::t -> if id = s then let (new_sv,res) = get_in_sv sv in (Signal(id,(emit,cv,new_sv,st))::t,res)
                                                     else let (new_si,res) = get_value t my_id id_thread s neutral in (Signal(id,(emit,cv,sv,st))::new_si,res)


    (* Vérifie si une liste est vide *)
    let isEmpty l = 
      match l with
        | [] -> true

        | _  -> false


    (* Créer un type *)
    let rec create s cons builders =
      let rec aux s nbrParam = 
        match (s,nbrParam) with
          | (new_stack,0)  -> ([],new_stack)

          | (Const c::t,nbr) -> let (res,new_s) = aux t (nbr-1) in (Const c::res,new_s)

          | (Type(c,v)::t,nbr) -> let (res,new_s) = aux t (nbr-1) in (Type(c,v)::res,new_s)

          | ([],nbr) -> if (nbr = 0) then ([],[]) else raise NotEnoughElem  

          | (_,_) -> raise FormatCreateInvalid
      in
      let rec inter nbrParam = let (res,new_stack) = aux s nbrParam in (Type(cons,res),new_stack)
      in  
      match builders with
        | []  -> raise BuilderNotFound 

        | Builder(nbrCons,nbrParam)::t   ->  if nbrCons = cons then let (res,new_s) = inter nbrParam in (res,Builder(nbrCons,nbrParam)::t,new_s)
                                                               else let (res,new_l,new_s) = create s cons t in (res,Builder(nbrCons,nbrParam)::new_l,new_s) 


    (* Compare deux types *)
    let rec compare t1 t2 env =
      match (t1,t2) with
        | (Type(c,v),Type(c1,v1))  ->   if c = c1 then Closure(("x",[Abstraction("y",[Variable "x";Constant 1;Ap])]),env)
                                                  else Closure(("x",[Abstraction("y",[Variable "y";Constant 1;Ap])]),env)

        | (_,_)                    ->   raise InvalidElemCompared


    (* Décompose un type par rapport à un pattern *)
    let rec destruct d values vars env l = 
      match (values,vars) with
       | ([],[])             ->   d

       | (value::t1,var::t)  ->   let new_d = destruct d t1 t env l in Save([value;P(var)],env,[Destruct],new_d,l) 

       | (_,_)               ->   raise FormatDestructInvalid


    (* Union de deux environnements *)
    let rec union env1 env2 = 
      match env1 with
        | []    ->   []

        | h::t  ->   if mem h env2 then union t env2 else union t (h::env2)




    (**** Machine ****)

    (* Applique une transition pour un état de la machine *)
    let transition machine =
      match machine with

        (*** Partie de base de la machine SECD ***)

          (* Constante *)
        | Machine(Thread(i,s,e,Constant b::c,d,l),tl,si,ip)                 ->   Machine(Thread(i,Const b::s,e,c,d,l),tl,si,ip) 

          (* Substitution *)
        | Machine(Thread(i,s,e,Variable x::c,d,l),tl,si,ip)                 ->   Machine(Thread(i,(substitution e x)::s,e,c,d,l),tl,si,ip) 

          (* Abstraction *)
        | Machine(Thread(i,s,e,Abstraction(x,c1)::c,d,l),tl,si,ip)          ->   Machine(Thread(i,Closure((x,c1),e)::s,e,c,d,l),tl,si,ip) 

          (* Application *)
        | Machine(Thread(i,v::Closure((x,c1),e1)::s,e,Ap::c,d,l),tl,si,ip)  ->   Machine(Thread(i,[],(add e1 x v false),c1,Save(s,e,c,d,l),l),tl,si,ip)

          (* Operation *)
        | Machine(Thread(i,s,e,Prim op::c,d,l),tl,si,ip)                    ->   Machine(Thread(i,(compute s op e),e,c,d,l),tl,si,ip)

          (* Récupération de sauvegarde *)
        | Machine(Thread(i,v::s,e,[],Save(s1,e1,c,d,l1),l),tl,si,ip)        ->   Machine(Thread(i,v::s1,e1,c,d,l1),tl,si,ip)



        (*** Partie pour la concurrence ***)

          (* Création thread *)
        | Machine(Thread(i,Closure((_,c1),e1)::s,e,Spawn::c,d,l),tl,si,ip)  ->   Machine(Thread(i,Const ip::s,e,c,d,l),append tl [Thread(ip,[],e1,c1,Empty,l)],si,ip+1)

          (* Initialisation d'un signal *)
        | Machine(Thread(i,s,e,Init::c,d,l),tl,si,ip)                       ->   let (id,new_si) = init si in 
                                                                                  Machine(Thread(i,Const id::s,e,c,d,l),tl,new_si,ip)

          (* Emission d'un signal *)
        | Machine(Thread(i,Const n::s,e,Emit::c,d,l),tl,si,ip)              ->   let (st,new_si) = emit si n in
                                                                                  Machine(Thread(i,s,e,c,d,l),append tl st,new_si,ip)

          (* Teste de présence *)
        | Machine(Thread(i,Closure((_,c2),e2)::Closure((_,c1),e1)::Const n::s,e,Present::c,d,l),tl,si,ip)
          -> if is_emit si n

                   (* Présence d'un signal *)
              then Machine(Thread(i,[],e1,c1,Save(s,e,c,d,l),l),tl,si,ip)
    
              else let new_si = stuck (Thread(i,Closure(("",c2),e2)::Closure(("",c1),e1)::Const n::s,e,Present::c,d,l)) n si in
              begin
                match tl with

                    (* Thread bloqué non remplacé *)
                  | []                            ->   Machine(Thread(ip,[],[],[],Empty,[]),[],new_si,ip+1)
                    
                    (* Thread bloqué remplacé *)
                  | Thread(i1,s1,e1,c1,d1,l1)::t  ->   Machine(Thread(i1,s1,e1,c1,d1,l1),t,new_si,ip)
              end

          (* Récupération dans la file d'attente *)
        | Machine(Thread(i,s,e,[],Empty,l),Thread(i1,s1,e1,c,d,l1)::tl,si,ip) 
          -> Machine(Thread(i1,s1,e1,c,d,l1),tl,si,ip)

          (* Fin d'un instant logique *)
        | Machine(Thread(i,s,e,[],Empty,l),[],si,ip)                        
          -> let (tl,new_si) = new_instant si in if isEmpty tl
            then match s with
      
                  | Const b::s1             ->   Machine(Thread(i,[Const b],[],[],Empty,[]),[],[],ip)
      
                  | Closure((x,c),env)::s1  ->   Machine(Thread(i,[Closure((x,c),env)],[],[],Empty,[]),[],[],ip)

                  | []                      ->   Machine(Thread(i,[],[],[],Empty,[]),[],[],ip)
      
                  | _                       ->   raise UnknowStackState

            else  Machine(Thread(i,s,e,[],Empty,l),tl,new_si,ip)


                                                                                  
        (*** Partie pour le partage de valeurs ***)

          (* Ajoute une valeur dans un signal *)
        | Machine(Thread(i,Const n::Const b::s,e,Put::c,d,l),tl,si,ip)      ->   let (st,new_si) = put_value si i n b in
                                                                                  Machine(Thread(i,s,e,c,d,l),append tl st,new_si,ip)

          (* Prend une valeur dans un signal *)
        | Machine(Thread(i,Const b::Const j::Const n::Closure((x,c1),e1)::s,e,Get::c,d,l),tl,si,ip)
          -> let (new_si,res) = get_value si i b j n in Machine(Thread(i,[],(add e1 x (Const res) false),c1,Save(s,e,c,d,l),l),tl,new_si,ip)




        (*** Partie pour la récursion ***)
        
          (* Récursion *)
        | Machine(Thread(i,Closure((f,[Abstraction(x,t)]),e1)::s,e,Fix::c,d,l),tl,si,ip) 
          -> Machine(Thread(i,Closure((x,t),(add e1 f (Closure((x,t),e1)) true))::s,e,c,d,l),tl,si,ip)
        




        (*** Partie types ***)
        
          (* Création d'un type *)
        | Machine(Thread(i,Const cons::s,e,Build::c,d,l),tl,si,ip)          ->   let (res,new_l,new_s) = create s cons l in 
                                                                                  Machine(Thread(i,res::new_s,e,c,d,new_l),tl,si,ip)

          (* Comparer deux types *)
        | Machine(Thread(i,t2::t1::s,e,Compare::c,d,l),tl,si,ip)            ->   let res = compare t1 t2 e in 
                                                                                  Machine(Thread(i,res::s,e,c,d,l),tl,si,ip)

        
          (* *)
        | Machine(Thread(i,Type(id1,values)::P(Pat(id,elems))::s,e,Destruct::c,d,l),tl,si,ip)
          -> let new_d = destruct (Save(s,e,c,d,l)) values elems e l in Machine(Thread(i,[],[],[],new_d,l),tl,si,ip) 


          (* *)                                                                       
        | Machine(Thread(i,v::P(Var x)::s,e,[Destruct],d,l),tl,si,ip)       ->   Machine(Thread(i,s,(add e x v false),[Destruct],d,l),tl,si,ip)         


          (* *)                                                                       
        | Machine(Thread(i,v::P(Var x)::s,e,Destruct::c,d,l),tl,si,ip)      ->   Machine(Thread(i,s,(add e x v false),c,d,l),tl,si,ip)         


          (*  *)
        | Machine(Thread(i,[],e,[Destruct],Save(s,e1,c,d,l1),l),tl,si,ip)   ->   Machine(Thread(i,s,(union e e1),c,d,l1),tl,si,ip)



        (*** Partie commune ***)

          (* Application neutre *)
        | Machine(Thread(i,s,e,Ap::c,d,l),tl,si,ip)                         ->   Machine(Thread(i,s,e,c,d,l),tl,si,ip)

          (* Récupération sauvegarde avec pile vide *)
        | Machine(Thread(i,[],e,[],Save(s,e1,c,d,l1),l),tl,si,ip)           ->   Machine(Thread(i,s,e1,c,d,l1),tl,si,ip)

        | _  -> raise Strange


    (* Applique les règles de la machine TTSI en affichant ou non les étapes *)
    let rec machine m afficher =
      match m with
          Machine(Thread(id,resultat,env,[],Empty,l),[],[],ip)  ->   printf "Le résultat est %s \n" (string_of_stack resultat)

        | ttsi                                                     ->   if afficher then print_machine ttsi else printf ""; machine (transition ttsi) afficher 
      
  

    (* Lance et affiche le résultat de l'expression *)
    let startTTSIv4 expression afficher = machine (Machine(Thread(0,[],[(false,"main",Const 0)],(convert_to_machine_language expression),Empty,[]),[],[(Signal(-1,(false,[],[],[])))],1)) afficher
    

  end