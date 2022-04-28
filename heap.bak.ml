type 'a t = {
  elts : 'a array;
  mutable length : int;
}

exception HeapOverflow;;
exception HeapEmpty;;

let make n v =
  { elts = Array.make n v; length = 0 };;

let father i =
  ((i+1)/2-1);;

let left i =
  2*(i+1)-1;;

let right i =
  2*(i+1);;

father 12;;
left 5;;
right 5;;
father 11;;

let nth heap i =
  heap.elts.(i);;

let length heap =
  heap.length;;

let add elt heap =
  if (Array.length heap.elts > heap.length) then
    begin
      let n = ref heap.length in
      heap.length <- heap.length + 1;
      heap.elts.(!n) <- elt;
      while ( !n <> 0 && heap.elts.(!n) > heap.elts.(father !n)) do 
    heap.elts.(!n) <- heap.elts.(father !n);
    heap.elts.(father !n) <- elt;
    n := father !n;
      done;
    end
  else raise HeapOverflow ;;

let max_with_sons heap i=
  print_int (left i);
  print_int (right i);
  let t = heap.length in
  print_int t;
  let x = heap.elts.(i) in
  let l = if (left i) < t then heap.elts.(left i) else -1 in
  let r = if (right i) < t then heap.elts.(right i) else -1 in
  print_int l;
  print_int r;
  if (l > r &&  r >= x) then left i else
    if  (l > x && x >= r) then left i else
      if ( r > l && l >= x ) then right i else
    if (r > x && x >= l) then right i else
      i;
  ;;


let take heap  =
  if heap.length <= 0 then raise (HeapEmpty);
  let racine = heap.elts.(0) in
  heap.length <- heap.length -1;
  let dernier = heap.elts.(heap.length) in
  heap.elts.(0) <- dernier;
  let i = ref 0 in
  let max = ref 0 in
  while  (!i <= heap.length && !max <> !i) do
    max := max_with_sons heap !i;
    heap.elts.(!i) <- heap.elts.(!max);
    heap.elts.(!max) <- dernier;
    i := !max ;
  done;
  racine;;

let peek heap =
  heap.elts.(0);;