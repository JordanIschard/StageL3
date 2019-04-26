open String ;;
open Printf ;;
open List ;;
open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open Ck.CKMachine ;;
open LangISWIM.ISWIM ;;


module CEKMachine =
  struct

    (**** Types ****)

    type  fermeture =  Fermeture of exprISWIM * (string *  fermeture) list

    
    type k_CEK = 
        Fun_CEK of  fermeture * k_CEK
      | Arg_CEK of  fermeture * k_CEK
      | Opd_CEK of ( fermeture list * operateur) * ( fermeture list) * k_CEK
      | MT_CEK










    (**** Exception ****)

    exception AucuneSubPossible











    (**** Affichage ****)

    (* Vérifie si le registre est vide *)
    let estMT_CEK mt =
      match mt with
          MT_CEK  ->   true
        
        | _       ->   false


    (* Convertit une  fermeture en chaîne de caractère *)
    let rec string_of_fermeture fermeture =
      let string_of_env env =
        match env with
          (var, fermeture)   ->   "("^var^" , "^(string_of_fermeture  fermeture)^")"
      in
      match fermeture with
        Fermeture(expr,env)  ->   "("^(string_of_expr expr)^" , {"^(concat_string_liste( map string_of_env env))^"} )"


    (* Convertit le registre en chaîne de caractère *)
    let rec string_of_registre_CEK registre =
      match registre with 
          (Fun_CEK( fermeture,mt))                             ->   if (estMT_CEK mt) then "(fun , "^(string_of_fermeture  fermeture)^" , mt)"
                                                                                      else "(fun , "^(string_of_fermeture  fermeture)^" , "^(string_of_registre_CEK mt)^")"
      
        | (Arg_CEK( fermeture,mt))                             ->   if (estMT_CEK mt) then "(arg , "^(string_of_fermeture  fermeture)^" , mt)"
                                                                                      else "(arg , "^(string_of_fermeture  fermeture)^" , "^(string_of_registre_CEK mt)^")"
       
        | (Opd_CEK((liste_fermeture,op),liste_fermeture1,mt))  ->   if (estMT_CEK mt) 
                                                                      then   "(opd , ["^(concat_string_liste( map string_of_fermeture liste_fermeture ))^", "^(string_of_operateur op)^"] , [ "
                                                                            ^(concat_string_liste( map string_of_fermeture liste_fermeture1 ))^"] , mt)"
                                                                      else   "(opd , ["^(concat_string_liste( map string_of_fermeture liste_fermeture ))^", "^(string_of_operateur op)^"] , [ "
                                                                            ^(concat_string_liste( map string_of_fermeture liste_fermeture1 ))^"] , "^(string_of_registre_CEK mt)^")"
                                                                          
        | MT_CEK                                               ->   "mt"
      

    (* Convertit un état de la machine CEK en chaîne de caractère *)
    let string_of_cek  fermeture registre = "("^(string_of_fermeture  fermeture)^" , "^(string_of_registre_CEK registre)^")\n" 

    (* Affiche un état de la machine CK *)
    let afficherCEK expression registre = printf "MachineCEK : %s" (string_of_cek expression registre)










    (**** Fonctions utiles ****)

    (* Convertit une expression en  fermeture *)
    let rec  fermeture_of_expr env liste =
      match liste with
          []    ->   []
        
        | h::t  ->   (Fermeture(h,env))::(fermeture_of_expr env t)


    (* Vérifie si une variable est dans l'environnement *)
    let rec estDansEnv env var =
      match env with
          []                    ->   false
        
        | (var1, fermeture)::t  ->   if (equal var1 var) then true else estDansEnv t var 


    (* Ajoute une variable et sa substitution dans l'environnement *)
    let ajoutEnv env varARemp  fermetureDeRemp = if (estDansEnv env varARemp) then env else append env [(varARemp, fermetureDeRemp)]


    (* Substitue une variable par sa  fermeture qui lui est assignée dans l'environnement *)
    let rec substitution var env =
      match env with
          []                    ->   raise AucuneSubPossible
        
        | (var1, fermeture)::t  ->   if( equal var1 var) then fermeture else substitution var t


    (* Vérifie si une  fermeture contient une constante *)
    let rec estConstFermeture  fermeture =
      match  fermeture with
          (Fermeture((Const const),env))  ->   true
        
        | _                               ->   false


    (* Convertit une liste de  fermeture, contenant des constantes, en une liste d'entier *)
    let rec convert_liste_fermeture_liste_int liste =
      match liste with
        []                                 ->   []
        
        | (Fermeture(Const const,env))::t  ->   const::(convert_liste_fermeture_liste_int t)
        
        | _                                ->   raise NotConstErreur
      
        








    (**** Machine CEK ****)

    (* Applique les règles de la machine CEK en affichant les étapes *)
    let rec machineCEK  fermeture registre = 
      let testRegistre registre  fermeture =
        match registre with
            (Fun_CEK(Fermeture((Abs(abs,expr1)),env),mt))        ->   machineCEK (Fermeture(expr1,(ajoutEnv env abs  fermeture))) mt

          | (Arg_CEK(Fermeture(expr1,env),mt))                   ->   machineCEK (Fermeture(expr1,env)) (Fun_CEK( fermeture,mt))

          | (Opd_CEK((liste_fermeture,op),liste_fermeture1,mt))  -> 
                                if (estVide liste_fermeture1)
                                  then 
                                    begin
                                      let newliste =  append [fermeture] liste_fermeture in
                                      if ( for_all estConstFermeture newliste) then machineCEK ( Fermeture(calcul op ( rev (convert_liste_fermeture_liste_int newliste)),[])) mt else raise EtatInconnu
                                    end
                                  else 
                                    begin
                                      let newliste =  append [fermeture] liste_fermeture in machineCEK (getPremElem liste_fermeture1) (Opd_CEK((newliste,op),(enleverTete liste_fermeture1),mt))
                                    end

          | _                                                    ->   raise EtatInconnu

      in

      afficherCEK  fermeture registre ;
      match ( fermeture,registre) with

        ( Fermeture(App(expr1,expr2),env),mt)       ->   machineCEK ( Fermeture(expr1,env)) (Arg_CEK(( Fermeture(expr2,env),mt))) 

        | ( Fermeture(Op(op,liste_expr),env),mt)    -> 
                                      begin
                                        match liste_expr with
                                            []      ->   raise FormatOpErreur
                                          
                                          | [h]     ->   machineCEK (Fermeture(h,env)) (Opd_CEK(([],op),[],mt))
                                          
                                          | h::t    ->   machineCEK (Fermeture(h,env)) (Opd_CEK(([],op),( fermeture_of_expr env t),mt))
                                      end

        | ( Fermeture(Const b,env),registre)        ->   if (estMT_CEK registre) then Const b else testRegistre registre (Fermeture(Const b,env))
    
        | ( Fermeture(Abs(abs,expr),env),registre)  ->   if (estMT_CEK registre) then Abs(abs,expr) else testRegistre registre ( Fermeture(Abs(abs,expr),env))
        
        | ( Fermeture((Var var),env),registre)      ->   machineCEK (substitution var env) registre


    (* Lance et affiche le résultat de l'expression *)
    let lancerCEK expression = printf "Le résultat est %s \n" (string_of_expr (machineCEK ( Fermeture(expression,[])) MT_CEK))

  end