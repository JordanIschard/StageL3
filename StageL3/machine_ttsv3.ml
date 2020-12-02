open String ;;
open Printf ;;
open List ;;
open Lang_tts.ISWIM ;;


(****** Ce module implante une 4ème version de la machine abstraite TTS.  *****)
module MachineTTS =
  struct

    (**** Types ****)

    (* identifiant *)
    type identifier = int 

    (* booléen représentant l'émission d'un signal *)
    type emit = bool 

    (* variable applicable *)
    type variable = string

    (* élément accepté par la chaîne de contrôle *)
    type element = 
      | Constant of int                              (* une constante   *)
      | Variable of variable                         (* une variable    *)
      | Abstraction of variable * element list       (* une abstraction *)

      | Ap                                           (* commande : appliquer                        *)
      | Prim of operateur                            (* commande : calculer                         *)
      | Spawn                                        (* commande : créer un thread                  *)
      | Present                                      (* commande : tester la présence d'un signal   *)
      | Init                                         (* commande : initialiser un signal            *) 
      | Emit                                         (* commande : émettre un signal                *)
      | Fix                                          (* commande : récurrence                       *)
      
    (* Représentation de la chaîne de contrôle *)
    type control = element list
    
    (* élément accepté dans la pile d'exécution *)
    type value =                                         
      | Const of int                                                            (* une constante *)
      | Closure of ((variable * control) * ( (bool * variable * value) list ))  (* une fermeture *)

    (* Représentation de la pile d'exécution *)
    type stack  = value list 

    (* Représentation de l'environnement, liste où l'on stocke les substitution *)
    type environment = (bool * variable * value) list 

    (* Représentation du dépôt, zone où l'on stocke la sauvegarde du thread courant *)
    type dump = 
      | Empty
      | Save of stack * environment * control * dump

    (* thread *)
    type thread = Thread of stack * environment * control * dump

    (* liste de threads *)
    type thread_list = thread list

    (* liste des threads bloqués *)
    type stuck = thread_list

    (* un signal *)
    type signal = Signal of identifier * (emit * stuck )

    (* liste de signaux *)
    type signal_list = signal list

    (* machine *)
    type machine = Machine of thread * thread_list * signal_list






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
    exception BadVersion
    exception SignalNotInit 





    (**** Convertir ****)

    (* Convertit le langage ISWIM en langage machine *)
    let rec convert_to_machine_language expression = 

      match expression with

          (* X *)                                                   (* [X] *)
        | Lang_tts.ISWIM.Var variable                          ->  [Variable variable]

          (* Abs(X,C) *)                                            (* [Abstraction(X,C)] *)
        | Lang_tts.ISWIM.Abs (variable,expr)                   ->  [Abstraction (variable,convert_to_machine_language expr)]

          (* App(C,C') *)                                           (* [ C ; C' ; Ap ] *)
        | Lang_tts.ISWIM.App (expr1,expr2)                     ->  append (convert_to_machine_language expr1) (append (convert_to_machine_language expr2) [Ap])

          (* Op(op,C) *)                                            (* [ [C] ; Prim op ] *)
        | Lang_tts.ISWIM.Op (op,expr_list)                     ->  append (flatten (map convert_to_machine_language expr_list)) [Prim op]

          (* b *)                                                   (* [b] *)
        | Lang_tts.ISWIM.Const const                           ->  [Constant const]

          (* Spawn(C) *)                                            (* [Abstraction("",C) ; Spawn ] *)
        | Lang_tts.ISWIM.Spawn expr                            ->  [Abstraction ("",convert_to_machine_language expr);Spawn]

          (* Present(X,C,C') *)                                     (* [ X ; Abstraction("",C) ; Abstraction("",C') ; Present ] *)
        | Lang_tts.ISWIM.Present (variable,expr1,expr2)        ->  [Variable variable;Abstraction("",convert_to_machine_language expr1);Abstraction("",convert_to_machine_language expr2);Present]

          (* Signal *)                                              (* [Init] *)
        | Lang_tts.ISWIM.Signal                                ->  [Init]

          (* Emit(X) *)                                             (* [ Neutral ; X ; Put ] *)
        | Lang_tts.ISWIM.Emit (variable)                       ->  [Variable variable;Emit]

          (* Wait *)                                                (* [ -1 ; Abstraction("",_) ; Abstraction("",_) ; Present ] *)
        | Lang_tts.ISWIM.Wait                                  ->  [Constant(-1);Abstraction("",[]);Abstraction("",[]);Present]

          (* Rec(X,C) *)                                            (* [ Abstraction(X,C) ; Fix ] *)
        | Lang_tts.ISWIM.Rec (variable,expr)                   ->  [Abstraction(variable,convert_to_machine_language expr);Fix]

          (* if(C,C',C'') *)                                         
        | Lang_tts.ISWIM.If (expr1,expr2,expr3)                ->  (append (append [ Abstraction("v",[Abstraction("t",[Abstraction("f1",[Variable "v";Variable "t";Ap;Variable "f1";Ap])])])]   
                                                                    (convert_to_machine_language expr1)) [Ap;Abstraction("",(convert_to_machine_language expr2)) ; Ap ; Abstraction("",(convert_to_machine_language expr3)) ; Ap])



    (**** Affichage ****)


    (* Convertit une liste en chaîne de caractère *)
    let rec string_of_a_list string_of_a l inter =
      match l with
        | []    ->   ""
        | [h]   ->   (string_of_a h)
        | h::t  ->   (string_of_a h)^inter^(string_of_a_list string_of_a t inter)
    

    (* Convertit un élément de la chaîne de contrôle en chaîne de caractères *)
    let rec string_of_element elem =
      match elem with                           
        | Constant const                       ->   (string_of_int const)
        | Variable variable                    ->   variable
        | Abstraction (variable,control_list)  ->   "<"^variable^","^(string_of_a_list string_of_element control_list " ")^">"

        | Ap                                   ->   "AP" 
        | Prim op                              ->   (string_of_operateur op)
        | Spawn                                ->   "SPAWN"
        | Present                              ->   "PRESENT"
        | Init                                 ->   "INIT" 
        | Emit                                 ->   "EMIT"         
        | Fix                                  ->   "FIX"        

    
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


    (* Convertit le thread en chaîne de caractères *)
    let string_of_thread t = 
      match t with
        | Thread(s,e,c,d)    ->   "\n  S  :   "^(string_of_stack s)
                                 ^"\n  E  :   "^(string_of_environment e)
                                 ^"\n  C  :   "^(string_of_control c)
                                 ^"\n  D  :   "^(string_of_dump d) 
    
    

    (* Convertit la liste de thread en chaîne de caractères *)
    let string_of_thread_list tl = string_of_a_list string_of_thread tl ","
        

    (* convertit un signal en chaîne de caractères *)
    let string_of_signal si =
      match si with
        | Signal(id,(emit,stuck))  ->   "<"^(string_of_int id)^",<"^(string_of_bool emit)^",{"^(string_of_thread_list stuck)^"}>>"


    (* convertit la liste des signaux en chaîne de caractères *)
    let string_of_signals si = string_of_a_list string_of_signal si " , "


    (* convertit la machine en chaîne de caractères *)
    let string_of_machine machine =
      match machine with
        Machine(t,tl,si)  ->       "T  :  "^(string_of_thread t)
                                ^"\nTL :  ["^(string_of_thread_list tl)^"]"
                                ^"\nSI :  ["^(string_of_signals si)^"]"

                                 
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

    
    (* Initialise un signal *)
    let rec init si = 
      match si with
        | []                  ->   (0,[Signal(0,(false,[]))])

        | [Signal(id,data)]   ->   (id+1,[Signal(id,data);Signal(id+1,(false,[]))])  

        | Signal(id,data)::t  ->   let (new_id,new_si) = init t in (new_id,Signal(id,data)::new_si)


    (* Vérifie si un signal est émis ou non *)
    let rec is_emit si id =
      match si with
        | []                        ->   raise SignalNotFound

        | Signal(id1,(e,_))::t  ->   if id = id1 then e else is_emit t id


    (* Bloque un thread qui attend un signal *)
    let rec stuck thread n si = 
      match si with
        | []                             ->   raise SignalNotFound

        | Signal(id,(emit,st))::t  ->   if id = n then Signal(id,(emit,append st [thread]))::t else Signal(id,(emit,st))::(stuck thread n t)


    (* Prend le choix du teste de présence indiquant la non émission du signal attendu *)
    let rec snd_choice st =
      match st with
        | []                                                         ->   []

        | Thread(Closure((_,c2),e2)::_::_::s,e,Present::c,d)::t      ->   Thread([],e2,c2,Save(s,e,c,d))::(snd_choice t)

        | _                                                          ->   raise InvalidFormatStuck


    (* Applique les modification nécessaire pour passer à l'instant suivant *)
    let rec new_instant si =
      match si with
        | []                             ->   ([],[])
        
        | Signal(id,(emit,st))::t  ->   let (tl,new_si) = new_instant t in (append (snd_choice st) tl,Signal(id,(false,[]))::new_si) 


    (* Vérifie si une liste est vide *)
    let isEmpty l = 
      match l with
        | [] -> true

        | _  -> false

      
    (* Emission *)
    let rec emit si signal =
      match si with
        | []                     ->   raise SignalNotInit 

        | Signal(id,(e,st))::t   ->   if id = signal then (st,Signal(id,(true,[]))::t) else let (tl,new_si) = emit t signal in (tl,Signal(id,(e,st))::new_si)




    (**** Machine ****)

    (* Applique une transition pour un état de la machine *)
    let transition machine =
      match machine with

        (*** Partie de base de la machine SECD ***)

          (* Constante *)
        | Machine(Thread(s,e,Constant b::c,d),tl,si)                      ->   Machine(Thread(Const b::s,e,c,d),tl,si) 


          (* Substitution *)
        | Machine(Thread(s,e,Variable x::c,d),tl,si)                      ->   Machine(Thread((substitution e x)::s,e,c,d),tl,si) 


          (* Abstraction *)
        | Machine(Thread(s,e,Abstraction(x,c1)::c,d),tl,si)               ->   Machine(Thread(Closure((x,c1),e)::s,e,c,d),tl,si) 


          (* Application *)
        | Machine(Thread(v::Closure((x,c1),e1)::s,e,Ap::c,d),tl,si)       ->   Machine(Thread([],(add e1 x v false),c1,Save(s,e,c,d)),tl,si)


          (* Operation *)
        | Machine(Thread(s,e,Prim op::c,d),tl,si)                         ->   Machine(Thread((compute s op e),e,c,d),tl,si)


          (* Récupération de sauvegarde *)
        | Machine(Thread(v::s,e,[],Save(s1,e1,c,d)),tl,si)                ->   Machine(Thread(v::s1,e1,c,d),tl,si)





        (*** Partie pour la concurrence ***)

          (* Création thread *)
        | Machine(Thread(Closure((_,c1),e1)::s,e,Spawn::c,d),tl,si)       ->   Machine(Thread(s,e,c,d),append tl [Thread([],e1,c1,Empty)],si)


          (* Initialisation d'un signal *)
        | Machine(Thread(s,e,Init::c,d),tl,si)                            ->   let (id,new_si) = init si in Machine(Thread(Const id::s,e,c,d),tl,new_si)


        | Machine(Thread(Const b::s,e,Emit::c,d),tl,si)                   ->   let (st,new_si) = emit si b in Machine(Thread(s,e,c,d),append tl st,new_si)

          (* Teste de présence *)
        | Machine(Thread(Closure((_,c2),e2)::Closure((_,c1),e1)::Const n::s,e,Present::c,d),tl,si)
          -> if is_emit si n

                   (* Présence d'un signal *)
              then Machine(Thread([],e1,c1,Save(s,e,c,d)),tl,si)
    
              else let new_si = stuck (Thread(Closure(("",c2),e2)::Closure(("",c1),e1)::Const n::s,e,Present::c,d)) n si in
              begin
                match tl with

                    (* Thread bloqué non remplacé *)
                  | []                         ->   Machine(Thread([],[],[],Empty),[],new_si)
                    
                    (* Thread bloqué remplacé *)
                  | Thread(s1,e1,c1,d1)::t     ->   Machine(Thread(s1,e1,c1,d1),t,new_si)
              end


          (* Récupération dans la file d'attente *)
        | Machine(Thread(s,e,[],Empty),Thread(s1,e1,c,d)::tl,si)          ->   Machine(Thread(s1,e1,c,d),tl,si)


          (* Fin d'un instant logique *)
        | Machine(Thread(s,e,[],Empty),[],si)                             ->   let (tl,new_si) = new_instant si in if isEmpty tl then  Machine(Thread(s,[],[],Empty),[],[])
                                                                                                                                 else  Machine(Thread(s,e,[],Empty),tl,new_si)


        (*** Partie pour la récursion ***)
        
          (* Récursion *)
        | Machine(Thread(Closure((f,[Abstraction(x,t)]),e1)::s,e,Fix::c,d),tl,si) 
          -> Machine(Thread(Closure((x,t),(add e1 f (Closure((x,t),e1)) true))::s,e,c,d),tl,si)





        (*** Partie commune ***)

          (* Application neutre *)
        | Machine(Thread(s,e,Ap::c,d),tl,si)                              ->   Machine(Thread(s,e,c,d),tl,si)


          (* Récupération sauvegarde avec pile vide *)
        | Machine(Thread([],e,[],Save(s,e1,c,d)),tl,si)                   ->   Machine(Thread(s,e1,c,d),tl,si)


        | _                                                               ->   raise Strange


    (* Applique les règles de la machine TTSI en affichant ou non les étapes *)
    let rec machine m afficher =
      match m with
        | Machine(Thread(resultat,env,[],Empty),[],[])  ->   Printf.printf "Le résultat est %s \n" (string_of_stack resultat)

        | ttsi                                          ->   if afficher then print_machine ttsi else Printf.printf ""; machine (transition ttsi) afficher 
      

    (* Lance et affiche le résultat de l'expression *)
    let startTTSv3 expression afficher = machine (Machine(Thread([],[],(convert_to_machine_language expression),Empty),[],[(Signal(-1,(false,[])))])) afficher
    

  end