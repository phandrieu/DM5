open FileIO;;
open PriorityQueue;;
type tree = Char of int | Node of tree*tree ;;

let taille_alphabet = 256;;

(*Fonctions et types auxiliaires*)


let creer_file_prio (tab: int array) : tree PriorityQueue.t = 
        let pq = PriorityQueue.make (taille_alphabet) (0, (Char 0)) in
        for a = 0 to taille_alphabet -1 do
                let f_a = tab.(a) in
                if f_a > 0 
                        then
                                PriorityQueue.add (Char a) (-f_a) pq (*il faut prendre toujours le plus petit*)
        done;
        pq
        ;;


(*Fonction auxiliaire : impression arbre -> Débug*)

let rec print_arbre tree = 
        match tree with
        | (Char c) -> Printf.printf "%c" (Char.chr c)
        | Node(t1, t2) -> 
                begin
                        Printf.printf "("; print_arbre t1; Printf.printf ","; print_arbre t2; Printf.printf ")"
                end;
        Printf.printf "\n"
;;

let rec print_pq (pq: tree PriorityQueue.t) = 
        for i = 0 to (pq.length -1) do
                match pq.elts.(i) with
                | (p, Char c) -> Printf.printf "%c" (Char.chr c)
                | (p, Node(t1, t2)) -> 
                        begin
                                Printf.printf "("; print_arbre t1; Printf.printf ","; print_arbre t2; Printf.printf ")"
                        end
                done
        
;;

let rec print_array tab = 
        Printf.printf "[";
        for i = 0 to (Array.length tab -1) do
                Printf.printf " %i ;" (tab.(i));
        done
;;

(*Fonctions principales*)
let build_tree tab : tree = 
        let pq  = (creer_file_prio tab) in
        while (pq.length >= 2) do
                let (pA1, a1) = PriorityQueue.take_full pq in
                let (pA2, a2) = PriorityQueue.take_full pq in
                let pn = pA1 + pA2 in
                let n = Node(a1, a2) in
                PriorityQueue.add n pn pq;
        done;
        let a = PriorityQueue.take pq in
        print_arbre a;
        a
;;

let codes (arbre : tree) : (bool list) array =
        let mescodes = Array.make taille_alphabet [] in
        let rec aux (a : tree) (acc: bool list) = 
                match a with
                |Char(c) -> mescodes.(c) <- List.rev acc;
                |Node(a1, a2) -> 
                                aux a1 (false::acc);
                                aux a2 (true::acc);
        in
        aux arbre [];
        mescodes
;;

let rec output_tree (out: FileIO.out_channel_bits) (arbre:tree) = 
        match arbre with
        |Char(c) -> 
                        begin 
                                FileIO.output_byte out 1;
                                FileIO.output_byte out c
                        end;
        |Node(l, r) -> begin
                        FileIO.output_byte out 0;
                        output_tree out l;
                        output_tree out r;
        end
;;

let rec input_tree (inp: FileIO.in_channel_bits) = 
        match (FileIO.input_byte inp) with
        |1 -> Char(FileIO.input_byte inp)
        |0 -> begin
                let x = input_tree inp in
                let y = input_tree inp in
                Node(x, y)
        end

        |_ -> failwith "Fin de l'arbre prématurée"
;;