
  (* Exercice 3 *)
type couleur = Pique | Coeur | Carreau |Trefle
type tete = Roi | Dame | Valet
type valeur = Tete of tete | Nombre of int 
type carte = {co : couleur ; va : valeur}
(* Question 1 *)
let transforme value = 
  match value with
  |Tete(Roi) -> 13 ;
  |Tete(Dame) -> 12 ;
  |Tete(Valet) -> 11 ;
  |Nombre(n) -> n ;;

let duel c1 c2 = 
  match (transforme(c1.va), transforme(c2.va)) with 
  |(a,b) when a > b -> 1 ;
  |(a,b) when a < b -> -1 ;
  |(a,b) when a = b -> 0 ;
  |others -> failwith ("something went wrong ") ;;

  (* Question 2 *)
let rec partie jeu1 jeu2 =
  match (jeu1, jeu2) with 
  |([],[]) -> 0 ;
  |([],l) | (l,[]) -> 0 ;
  |(t1 :: q1, t2 :: q2) -> duel(t1) (t2) + partie(q1) (q2) ;;
  