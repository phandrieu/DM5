open PriorityQueue;;
open FileIO;;

type tree = Char of int | Node of tree * tree;;

let taille_alphabet = 256;;

let build_occtab (txttab : int array) =
  let occtab = (Array.make taille_alphabet 0) in
  let n = Array.length txttab in

  for i = 0 to (n-1) do
    occtab.( txttab.(i) ) <- occtab.( txttab.(i)) + 1;
  done;

  occtab
;;

let _combien_diff (occtab : int array) =
  let n = Array.length occtab in
  let s = ref 0 in

  for i = 0 to (n-1) do
    if occtab.(i) <> 0 then s := (!s) + 1;
  done;

  !s
;;               
                                      
  
let build_tree (occtab : int array) =
  Printf.printf "=======================================\n\n";
  let n = _combien_diff occtab in
  
  (* remplissage de la foret initiale *)
  let foret = PriorityQueue.make n (0, (Char 0)) in (*revoir .make, taille/mutabilit� ?? *)
  for i = 0 to (taille_alphabet-1) do
    if occtab.(i) <> 0 then
      PriorityQueue.add (Char i)  (-occtab.(i)) foret;
  done;
  
  while foret.length >= 2 do
    let (na1, a1) = PriorityQueue.take_full foret in
    let (na2, a2) = PriorityQueue.take_full foret in
    Printf.printf "Nouveau : %i + %i\n" na1 na2;
    
    PriorityQueue.add (Node (a1, a2)) (na1 + na2) foret;
  done;

  (PriorityQueue.take foret)
;;


let right (abr : tree) =
  match abr with
  | Char (_) -> failwith "HuffmanTree.right : feuille"
  | Node (_, d) -> d
;;


let left (abr : tree) =
  match abr with
  | Char (_) -> failwith "Huffmantree.left : feuille"
  | Node (g, _) -> g
;;


let codes (arbre : tree) =
  let codtab = Array.make taille_alphabet [] in

  let rec _parcourirArbre (abr : tree) (code : bool list) =
    match abr with
    | Char (a) -> codtab.(a) <- (List.rev code)
    | Node (ssg, ssd) -> (
      _parcourirArbre ssg (true :: code);
      _parcourirArbre ssd (false :: code);
    )
  in

  _parcourirArbre arbre [];
  codtab
;;



let rec output_tree (file : FileIO.out_channel_bits) (abr : tree) =
  (* c'est un parcour pr�fixe *)
  match abr with
  | Char (a) -> (
    Printf.printf "\n1\t%i" a;
    FileIO.output_byte file 1;
    FileIO.output_byte file a)
  | Node (ssg, ssd) -> (
    Printf.printf "\n0";
    FileIO.output_byte file 0;
    output_tree file ssg;
    output_tree file ssd; )
;;


exception Intrus_Dans_Larbre;;

let rec input_tree (file : FileIO.in_channel_bits) =
  match (FileIO.input_byte file) with
  | 1 -> (
    let car = FileIO.input_byte file in
    Printf.printf "\n1\t%i" car;
    Char (car))
  | 0 -> Node (input_tree file, input_tree file)
  | _ -> raise Intrus_Dans_Larbre
;;
