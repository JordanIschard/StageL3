open String ;;
open Printf ;;
open List ;;
open Machine_cc.CCMachine ;;
open Machine_ck.CKMachine ;;
open Iswim.ISWIM ;;


module CEKMachine =
  struct

    (**** Types ****)

    (* Type représentant une fermeture *)
    type  fermeture =  Fermeture of expr * (string *  fermeture) list

    (* Type représentant la continuation *)
    type k = 
        Fun of  fermeture * k
      | Arg of  fermeture * k
      | Opd of ( fermeture list * operateur) * ( fermeture list) * k
      | MT

    (* Type représentant la machine CEK *)
    type machine = Machine of fermeture * k





    (**** Exception ****)

    exception AucuneSubPossible





    (**** Affichage ****)


    (* Convertit une  fermeture en chaîne de caractère *)
    let rec string_of_fermeture fermeture =
      let string_of_env env =
        match env with
          (var, fermeture)   ->   "("^var^" , "^(string_of_fermeture  fermeture)^")"
      in
      match fermeture with
        Fermeture(expr,env)  ->   "("^(string_of_expr expr)^" , {"^(concat_string_liste( map string_of_env env))^"} )"


    (* Convertit le registre en chaîne de caractère *)
    let rec string_of_registre registre =
      match registre with 
          Fun(fermeture,mt)                              ->   "(fun , "^(string_of_fermeture  fermeture)^" , "^(string_of_registre mt)^")"
      
        | Arg(fermeture,mt)                              ->   "(arg , "^(string_of_fermeture  fermeture)^" , "^(string_of_registre mt)^")"
       
        | Opd((liste_fermeture,op),liste_fermeture1,mt)  ->   "(opd , ["^(concat_string_liste( map string_of_fermeture liste_fermeture ))^", "^(string_of_operateur op)^"] , [ "
                                                                            ^(concat_string_liste( map string_of_fermeture liste_fermeture1 ))^"] , "^(string_of_registre mt)^")"
                                                                          
        | MT                                             ->   "mt"
      

    (* Convertit un état de la machine CEK en chaîne de caractère *)
    let string_of_machine  machine = 
      match machine with 
        Machine(fermeture,registre)  ->   "("^(string_of_fermeture  fermeture)^" , "^(string_of_registre registre)^")\n" 

        
    (* Affiche un état de la machine CEK *)
    let afficherCEK machine = printf "MachineCEK : %s" (string_of_machine machine)






    (**** Fonctions utiles ****)

    (* Convertit une expression en  fermeture *)
    let rec  fermeture_of_expr env liste =
      match liste with
          []    ->   []
        
        | h::t  ->   (Fermeture(h,env))::(fermeture_of_expr env t)


    let rec ajoutEnv env varARemp fermetureDeRemp =
      match env with
          []                   ->   [(varARemp, fermetureDeRemp)]
        
        | (var1,fermeture)::t  ->   if (equal var1 varARemp) then append [(var1,fermetureDeRemp)] t else append [(var1,fermeture)] (ajoutEnv t varARemp fermetureDeRemp) 

        
    (* Vérifie si une  fermeture contient une constante *)
    let rec estConstFermeture  fermeture =
      match  fermeture with
          (Fermeture((Const const),env))  ->   true
        
        | _                               ->   false


    (* Convertit une liste de  fermeture, contenant des constantes, en une liste d'entier *)
    let rec convert_liste_fermeture_liste_int liste =
      match liste with
        []                               ->   []
        
        | (Fermeture(Const const,_))::t  ->   const::(convert_liste_fermeture_liste_int t)
        
        | _                              ->   raise NotConstErreur
      
    
    (* Retourne le 1er élément d'une liste *)
    let getPremElem liste env = 
      match liste with
        | []    ->   raise FormatOpErreur
        
        | h::t  ->   (Fermeture(h,env),(map (fun x -> Fermeture(x,env)) t))


    (* Substitue une variable par sa  fermeture qui lui est assignée dans l'environnement *)
    let rec substitution var env =
      match env with
          []                    ->   raise AucuneSubPossible
        
        | (var1, fermeture)::t  ->   if(equal var1 var) then fermeture else substitution var t






    (**** Machine CEK ****)

    (* Applique une transition de la machine CEK pour un état donné *)
    let transition machine =
      match machine with
          Machine(Fermeture(App(expr1,expr2),env),mt)                        ->   Machine(Fermeture(expr1,env),Arg(Fermeture(expr2,env),mt))

        | Machine(Fermeture(Op(op,liste_expr),env),mt)                       ->   let (expr,new_liste) = getPremElem liste_expr env in Machine(expr,Opd(([],op),new_liste,mt))

        | Machine(Fermeture(Var var,env),mt)                                 ->   Machine((substitution var env),mt)

        | Machine(Fermeture(var,env),Arg(Fermeture(expr,env1),mt))           ->   Machine(Fermeture(expr,env1),Fun(Fermeture(var,env),mt))

        | Machine(Fermeture(var,env),Fun(Fermeture(Abs(abs,expr),env1),mt))  ->   Machine(Fermeture(expr,(ajoutEnv env1 abs (Fermeture(var,env)))),mt)

        | Machine(Fermeture(var,env),Opd((liste_ferm,op),[],mt))             ->   let new_liste = Fermeture(var,env)::liste_ferm in
                                                                                     if (for_all estConstFermeture new_liste)
                                                                                        then Machine(Fermeture(calcul op (rev (convert_liste_fermeture_liste_int new_liste)),[]),mt)
                                                                                        else raise EtatInconnu

        | Machine(Fermeture(var,env),Opd((liste_ferm,op),h::t,mt))           ->   Machine(h,Opd((Fermeture(var,env)::liste_ferm,op),t,mt))

        | _                                                                  ->   raise EtatInconnu


    (* Applique les règles de la machine  en affichant les étapes *)
    let rec machine cek afficher= 
      match cek with
          Machine(Fermeture(Const b,_),MT)        ->   Const b
        
        | Machine(Fermeture(Abs(abs,expr),_),MT)  ->   Abs(abs,expr)

        | indetermine                               ->   if (afficher) then (afficherCEK indetermine) else printf ""; machine (transition indetermine) afficher
    

    (* Lance et affiche le résultat de l'expression *)
    let lancerCEK expression afficher = printf "Le résultat est %s \n" (string_of_expr (machine (Machine(Fermeture(expression,[]),MT)) afficher))

  end