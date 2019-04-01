

(********************* Le type et quelques définition **************************)

(* Type terme qui est soit une variable Var,
   soit un niveau astraction Abs,
   soit une application entre deux termes
   Les variables sont représentées par des chaines caractères 
 *)
type term = Var of string | Abs of string * term  | App of term * term;;

(* retourne le première élément *)
let vrai = Abs("x",Abs("y",Var "x"));;

(* retourne le second élément *)
let faux =  Abs("x",Abs("y",Var "y"));;

(* v est le booléen, t la solution choisie si vrai et f la solution choisie si false *)
let si = Abs("v",Abs("t",Abs("f",App(App(Var "v",Var "t"),Var "f"))));;


(********************* Fonction pratiques ***************************)



(* Donne l'ensemble des variables liées du terme *)
let rec lie term =
  match term with
    Var var -> []
   |Abs (el,t) -> el::lie t
   |App (t1,t2) -> List.append (lie t1) (lie t2)
;;

(* Affiche une liste de string (Provisoire)*)
let rec afficherList liste =
  match liste with
    [] -> Printf.printf "\n"
  | e::t -> Printf.printf "%s ;" e ; afficherList t
;;

(* Donne l'ensemble des variables libres du terme *)
let libre term =
  let varlie = lie term in
  let rec aux term =
    match term with
      Var var -> if (List.mem var varlie) then [] else [var]
     |Abs (el,t) -> aux t
     |App (t1,t2) -> List.append (aux t1) (aux t2)
  in aux term
;;


(* Retourne un booléen représentant l'égalité de deux termes *)
let rec equals_terme term1 term2 =
  match (term1,term2) with
    (Var var1,Var var2) -> (String.equal var1 var2)
                        
  | (Abs(var1,terme1),Abs(var2,terme2)) -> if (String.equal var1 var2)
                                                   then (equals_terme terme1 terme2)
                                                   else false
  | (App(terme1,terme2),App(terme3,terme4)) -> (equals_terme terme1 terme3)
                                               && (equals_terme terme2 terme4)
  | (_,_) -> false
;;

(* Retire l'élément donné en paramètre *)
let retirerEl var liste =
  let rec aux liste =
    match liste with
    |[] -> []
    |h::t -> if (String.equal var h)
             then aux t
             else h::(aux t)
  in aux liste
;;

(* Donne un nouveau nom de var (À améliorer pour éviter la liste finie)*)
let choisirVar var =
  let variables = ["x";"y";"z";"w"] in
  let rec aux variables =
    match variables with
    |[] -> "lkihgoihoi"
    |h::t -> if (String.equal var h)
             then aux t
             else h
  in aux variables
;;

(* Renomme la variable pour qu'elle satisfasse les contrainte de la béta-réduction *)
let renommer var terme1 terme2 =
  let rec aux var varlibre  =
    if (List.mem var varlibre) then (aux (choisirVar var) varlibre) else var
  in aux var (retirerEl var (List.append (libre terme1) (libre terme2)))
;;


(*************************LA REDUCTION**************************************)

(* 
   Réduit le terme en remplaçant la variable à remplacer par le terme 
   de remplacement en suivant les règles de béta-réduction 
*)
let rec reduireb varARemp terme termeDeRemp =

  (* On regarde à quoi ressemble le terme *)
  match terme with

    (* Le terme est une variable du coup on teste si le terme est égal à la variable à remplacer *)
    Var var -> if (String.equal var varARemp)

               (* C'est le cas du coup on remplace *)
              then termeDeRemp

               (* Ce n'est pas le cas du coup on ne remplace pas*)
               else Var var

  (* Le terme est une application du coup on cherche dans ces deux termes si on peut béta-réduire *)
  | App(terme1 , terme2) -> App((reduireb varARemp terme1 termeDeRemp)
                               ,(reduireb varARemp terme2 termeDeRemp))

  (* 
     Le terme est une abstraction du coup on teste si la variable de 
     l'abstraction est égal à la variable à remplacer
   *)
  | Abs(var,terme1) -> if (String.equal var varARemp)

                       (* C'est le cas du coup on ne remplace pas *)
                       then Abs(var,terme1)

                       (* Ce n'est pas le cas du coup on remplace *)
                       else
                         (* On commence par renommer var *)
                         let newvar = (renommer var terme1 termeDeRemp) in
                         
                         Abs(newvar,
                           
                             (* On remplace var par le terme de remplacement *)
                             (reduireb varARemp
                                
                                (* On remplace l'ancien var par newvar dans le terme *)
                                (reduireb var terme1 (Var newvar))
                                
                              termeDeRemp )
                           )
;;

(* Donne le terme avec ZERO ou UNE béta-réduction faite sur le terme *)  
let rec beta_red terme =
  
  match terme with

    (*
      On a une application contenant une abstraction et un terme, 
      c'est le cas que l'on cherche du coup on applique la béta-réduction
     *)
    App(Abs (var,term1),term2) -> (reduireb var term1 term2)

   (* 
      On a une application contenant deux termes générique,
      on applique beta-red sur ces deux termes à la recherche d'une réduction possible 
    *)
   |App(term1,term2) -> App((beta_red term1),(beta_red term2))

   (* 
      On a une abstraction du coup on applique beta_red sur term 
      à la recherche d'une réduction possible 
    *)
   |Abs(var,term) -> Abs(var,(beta_red term))

   (* 
      On a une variables on ne peut pas chercher une réduction possible
      sur une variable du coup on fait rien
    *)
   |Var var -> Var var
;;


(* Donne le terme avec ZERO ou UNE êta réduction faite sur le terme *)
let rec eta_red terme =
  
  match terme with
    
    (* Le terme est une variable donc pas d'eta-réduction*)
    Var var -> Var var
             
   (* 
      Le terme est une application ducoup on regarde dans ces deux termes si 
      une  eta-réduction est possible 
    *)
   |App (t1,t2) -> App((eta_red t1),(eta_red t2))

   (*
     Le terme est une abstraction, c'est ce qui nous intéresse on regarde si on a la forme
     recherchée
    *)
   |Abs (el,t) -> match t with
   
                  (* On regarde t le terme lié à l'abstraction et on a une application *)             
                  | App(t1,Var var) ->
                     
                     (* 
                       On vérifie que el est égal à la variable var et que el n'est pas une 
                       variable libre de t1
                      *)
                     if ((String.equal el var) && (not(List.mem el (libre t1))))

                     (* On a toute les conditions réunit donc on réduit *)
                     then (eta_red t1)

                     (* On est pas dans les conditions nécessaires donc on ne réduit pas *)
                     else Abs(el,(eta_red t))

                   (* On a rien trouvé d'intéressant donc on ne réduit pas *)
                  | _ -> Abs(el,(eta_red t))
;;

let rec n_red terme =
  let red = eta_red (beta_red terme) in
  if (equals_terme terme red) then red else n_red red
;;

(* A faire 
 aplha-réduction
 parser
*)


(******************** LES TEST ********************)

let termtest = Abs("coucou",(App(Var "patrick" , Abs("tu" ,Var "coucou"))));;
let termeta = App(Abs("x",App(Abs("z",Var "z"),Var "y")),(Abs("x",Var "w")));;
let termbeta = App(Abs("x",Var "x"),App(Var "y",Var "u"));;
let termbeta2 = App(Var "x",Var "y");;
let term1 =  App(Abs("x",Var "x"),Var "y");;
let termbeta1 = App(Abs("y",(App(term1,termbeta2))),Var "z");;
let term2 = App(
                Abs("x",
                    Abs("y",App(App(Var "y",Var "z"), Var "x")))
               ,Var "z"
              );;



eta_red (eta_red termeta);;

afficherList ["coucou";"liho";"kugiu"];;

equals_terme termeta termtest;;

libre termtest;;

libre termeta;;
retirerEl "y" (libre termeta);;

lie termtest;;

libre  (Abs("y",App(App(Var "y",Var "z"), Var "x")));;
beta_red term2;;


n_red term2;;
