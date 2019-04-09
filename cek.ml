open Cc.CCMachine ;;
open Scc.SCCMachine ;;
open Ck.CKMachine ;;
open LangISWIM.ISWIM ;;


module CEKMachine =
  struct

    (**** Types ****)

    type clause = Clause of exprISWIM * (string * clause) list

    
    type k_CEK = 
      Fun_CEK of clause * k_CEK
      | Arg_CEK of clause * k_CEK
      | Opd_CEK of (clause list * operateur) * (clause list) * k_CEK
      | MT_CEK


    (**** Exception ****)

    exception AucuneSubPossible

    (**** Fonctions utiles ****)

    (* Vérifie si le registre est vide *)
    let estMT_CEK mt =
      match mt with
        MT_CEK -> true
        | _ -> false


    (* Convertit une clause en chaîne de caractère *)
    let rec string_of_clause clause =
      let string_of_env env =
        match env with
         (var,clause) -> "("^var^" , "^(string_of_clause clause)^")"
      in
      match clause with
       Clause(expr,env) -> "("^(string_of_expr expr)^" , {"^(concat_string_liste(List.map string_of_env env))^"} )"

    (* Convertit le registre en chaîne de caractère *)
    let rec string_of_registre_CEK registre =
      match registre with 
        (Fun_CEK(clause,mt)) -> if (estMT_CEK mt) then "(fun , "^(string_of_clause clause)^" , mt)"
                                        else "(fun , "^(string_of_clause clause)^" , "^(string_of_registre_CEK mt)^")"
        | (Arg_CEK(clause,mt)) -> if (estMT_CEK mt) then "(arg , "^(string_of_clause clause)^" , mt)"
                                          else "(arg , "^(string_of_clause clause)^" , "^(string_of_registre_CEK mt)^")"
        | (Opd_CEK((liste_clause,op),liste_clause1,mt)) -> if (estMT_CEK mt) 
        then "(opd , ["^(concat_string_liste(List.map string_of_clause liste_clause ))^", "^(string_of_operateur op)^"] , [ "^(concat_string_liste(List.map string_of_clause liste_clause1 ))^"] , mt)"
        else "(opd , ["^(concat_string_liste(List.map string_of_clause liste_clause ))^", "^(string_of_operateur op)^"] , [ "^(concat_string_liste(List.map string_of_clause liste_clause1 ))^"] , "^(string_of_registre_CEK mt)^")"
        | MT_CEK -> "mt"
      
    
    (* Convertit un état de la machine CEK en chaîne de caractère *)
    let string_of_cek clause registre =
      "("^(string_of_clause clause)^" , "^(string_of_registre_CEK registre)^")\n" 

    (* Affiche un état de la machine CK *)
    let afficherCEK expression registre =
        Printf.printf "MachineCEK : %s" (string_of_cek expression registre)

    (* Convertit une expr en clause *)
    let rec clause_of_expr env liste =
      match liste with
        [] -> []
        | h::t -> (Clause(h,env))::(clause_of_expr env t)

    (* Vérifie si une variable est dans l'environnement *)
    let rec estDansEnv env var =
      match env with
        [] -> false
        | (var1,clause)::t -> 
            if (String.equal var1 var)
              then true
              else estDansEnv t var 

    (* Ajoute une variable et sa substitution dans l'environnement *)
    let ajoutEnv env varARemp clauseDeRemp =
      if (estDansEnv env varARemp)
        then env
        else List.append env [(varARemp,clauseDeRemp)]

    (* Substitue une variable par sa clause qui lui est assignée dans l'nevironnement *)
    let rec substitution var env =
      match env with
        [] -> raise AucuneSubPossible
        | (var1,clause)::t -> 
            if(String.equal var1 var) 
              then clause
              else substitution var t

    (* Vérifie si une clause contient une constante *)
    let rec estConstClause clause =
      match clause with
        (Clause((Const const),env)) -> true
        | _ -> false

    (* Convertit une liste de clause, contenant des constantes, en une liste d'entier *)
    let rec convert_liste_clause_liste_int liste =
      match liste with
        [] -> []
        | (Clause(Const const,env))::t -> const::(convert_liste_clause_liste_int t)
        | _ -> raise NotConstErreur
        
    (**** Machine CEK ****)

    (* Applique les règles de la machine CEK en affichant les étapes *)
    let rec machineCEK clause registre = 
      let testRegistre registre clause =
        match registre with

          (Fun_CEK(Clause((Abs(abs,expr1)),env),mt)) -> machineCEK (Clause(expr1,(ajoutEnv env abs clause))) mt

          | (Arg_CEK(Clause(expr1,env),mt)) -> machineCEK (Clause(expr1,env)) (Fun_CEK(clause,mt))

          | (Opd_CEK((liste_clause,op),liste_clause1,mt)) -> 
            if (estVide liste_clause1)
              then 
                begin
                  let newliste = List.append [clause] liste_clause in
                  if (List.for_all estConstClause newliste)
                    then machineCEK (Clause(calcul op (List.rev (convert_liste_clause_liste_int newliste)),[])) mt
                    else raise EtatInconnu
                end

              else 
                begin
                  let newliste = List.append [clause] liste_clause in
                  machineCEK (getPremElem liste_clause1) (Opd_CEK((newliste,op),(enleverTete liste_clause1),mt))
                end

          | _ -> raise EtatInconnu

      in

      afficherCEK clause registre ;

      match (clause,registre) with

      (Clause(App(expr1,expr2),env),mt) -> machineCEK (Clause(expr1,env)) (Arg_CEK((Clause(expr2,env),mt))) 

      | (Clause(Op(op,liste_expr),env),mt) -> 
          begin
            match liste_expr with
              [] -> raise FormatOpErreur
              | [h] -> machineCEK (Clause(h,env)) (Opd_CEK(([],op),[],mt))
              | h::t -> machineCEK (Clause(h,env)) (Opd_CEK(([],op),(clause_of_expr env t),mt))
          end

      | (Clause(Const b,env),registre) -> 
          if (estMT_CEK registre)
            then Const b 
            else testRegistre registre (Clause(Const b,env))
  
      | (Clause(Abs(abs,expr),env),registre) -> 
          if (estMT_CEK registre)
            then Abs(abs,expr) 
            else testRegistre registre (Clause(Abs(abs,expr),env))
      
      | (Clause((Var var),env),registre) -> machineCEK (substitution var env) registre

    (* Lance et affiche le résultat de l'expression *)
    let lancerCEK expression =
      Printf.printf "Le résultat est %s \n" (string_of_expr (machineCEK (Clause(expression,[])) MT_CEK))

  end