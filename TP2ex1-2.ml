(* Exercice 1 *)
let rec vectoriser f l = 
  match l with 
  |[] -> [] ;
  | head :: queue -> [f head]  @ vectoriser (f) (queue) ;;

  let l = [1; 2; 3; 4; 5] ;;
  let f x = x + 1;;

let  liste_est_pair l = vectoriser (fun x -> x mod 2 = 0) l ;;

let rec select c l = 
  match l with 
  | [] -> [] ; 
  |head :: queue -> 
    match c head with
    | true -> [head] @ select c queue ;
    | false -> select c queue ;;
    
let rec pour_tout p l =
  match l with 
  | [] -> true ;
  | head :: queue -> p head && pour_tout p queue ;;

let rec il_existe p l = 
  match l with 
  | [] -> true ;
  | head :: queue -> p head || pour_tout p queue ;;

let rec compose l x = 
  match l with 
  | [] -> x ; 
  | [g] -> g x ; 
  | head :: queue -> head(compose queue x) ;;

let rec applique l x =
  match l with 
  | [] -> x;
  | [g] -> g x ;
  | head :: queue -> applique queue (head x) ;;

let rec est_sans_doublon_triee l = 
  match l with 
  |[] -> true ;
  |[x] -> true ;
  |head :: mid :: queue -> (head = mid) && est_sans_doublon_triee(mid :: queue) ;;

let rec supprime_doublons_triee l = 
  match l with 
  |[] -> [] ;
  |[x] -> [x];
  |head :: mid :: queue -> 
    match head = mid with 
    |true -> supprime_doublons_triee (mid :: queue); 
    |false -> head :: supprime_doublons_triee(mid :: queue) ;;

let rec longueur list= 
  match list with
  |[] -> 0
  | head :: queue -> 1 + longueur queue ;;
let rec decode_tuple tuple = 
  match tuple with
  |(0, x) -> [];
  |(n, x) -> x :: decode_tuple (n-1, x) ;;

let rec decode list = 
  match list with 
  |[] -> []
  |head :: queue -> decode_tuple(head) @ decode queue ;;

let rec encode_aux list = 
  match l with
  |[] -> [] ; 
  |head :: queue -> (head, 1) :: encode_aux (queue) ;;


(* Exercice 2 *)


type expr =  
|Nombre of int
|Plus of (expr * expr)
|Moins of (expr * expr)
|Fois of (expr * expr)
let est_valeur expr = 
  match expr with 
  |Nombre(n) -> true 
  |Plus(n1, n2) | Moins(n1,n2) | Fois(n1,n2) -> false;;



let rec nombre_operateurs expr =
  match expr with
  |Nombre(_) -> 0; 
  |Plus(n1, n2) |Moins(n1,n2) |Fois(n1,n2) -> 1 + nombre_operateurs(n1) + nombre_operateurs(n2) ;; 

let rec eval expr = 
  match expr with
  |Nombre(n) -> n;
  |Plus(n1,n2) -> (eval(n1) + eval(n2))
  |Moins(n1,n2) -> (eval(n1) - eval(n2))
  |Fois(n1,n2) -> (eval(n1) * eval(n2)) ;; 

let rec hauteur expr =
  match expr with
  |Nombre(n) -> 0;
  |Plus(n1,n2)|Moins(n1,n2)|Fois(n1,n2) -> 1 + max(hauteur(n1))(hauteur(n2)) ;; 

let rec to_string expr = 
  match expr with 
  |Nombre(n) -> string_of_int(n) ;
  |Plus(n1,n2) -> "(" ^ to_string(n1) ^ "+" ^ to_string(n2) ^ ")";
  |Moins(n1,n2) -> "(" ^ to_string(n1) ^ "-" ^ to_string(n2) ^ ")";
  |Fois(n1,n2) -> "(" ^ to_string(n1) ^ "*" ^ to_string(n2) ^ ")";;

(* Tests *)
let n1 = Nombre(3) ;;
let n2 = Nombre(5) ;;
let e1 = Plus(n1, n2) ;;
let e2 = Moins(n1, n2) ;;
let e3 = Fois(e1, e2) ;;




