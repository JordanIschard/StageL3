

module Stack = struct
  type 'a t = 'a list ref

  exception Stack_empty

  (** Retourne une nouvelle pile vide *)
  let create () : 'a t = ref []

  (** Indique si la pile s est vide *)
  let is_empty (s : 'a t) : bool =
    !s = []

  (** Retourne la valeur du sommet de la pile s *)
  let peek (s : 'a t) : 'a =
    match !s with
    | [] -> raise Stack_empty
    | x :: xq -> x

  (** Ajoute la valeur x sur le sommet de la pile s *)
  let push (s : 'a t) (x : 'a) : unit =
    s := x :: !s

  (** Supprime et retourne la valeur du sommet de la pile s *)
  let pop (s : 'a t) : 'a =
    match !s with
    | [] -> raise Stack_empty
    | x :: xq -> s := xq ; x
end

