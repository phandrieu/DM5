type 'a t = {
        elts : 'a array;
        mutable length : int;
};;

exception HeapOverflow;;
exception HeapEmpty;;

let make (t:int) e = 
        {elts = (Array.make t e) ; length = 0};;

let father i = 
        if i = 0
        then failwith "Heap.father : lecture du père de la racine." 
        else ((i+1)/2 -1);;

let left i = 
        2*(i+1)-1;;

let right i = 
        2*(i+1);;

let nth tas i = 
        tas.elts.(i);;

let length tas =
        tas.length;;


let swap tab i j = 
  let temp = tab.(j) in 
  tab.(j) <- tab.(i);
  tab.(i) <- temp;
;;

let add valeur tas =
  let n = ref tas.length in 
  if !n = (Array.length tas.elts) then 
    raise HeapOverflow;

  tas.length <- !n + 1;
  tas.elts.(!n) <- valeur; 
  while (!n > 0 && tas.elts.(father !n) < tas.elts.(!n)) do 
    swap tas.elts !n (father !n);
    n := father !n;
  done;
;;

let max_with_sons tas i =
        if (tas.elts.(left i)) < (tas.elts.(right i))
                then (right i)
        else (left i)
      ;;

let take (tas : 'a t) : 'a =
        if tas.length = 0 then
                raise HeapEmpty;
        let racine = tas.elts.(0) in
        let x = tas.elts.(tas.length - 1) in
        tas.elts.(0) <- x;
        tas.length <- (tas.length - 1);
        let n = ref 0 in
        while ((left !n < tas.length) && (right !n < tas.length) && (!n < max_with_sons tas !n)) do
                swap tas.elts !n (max_with_sons tas !n);
                n := max_with_sons tas !n;
        done;
        racine
;;



let peek tas = 
        tas.elts.((length tas) -1)
;;
