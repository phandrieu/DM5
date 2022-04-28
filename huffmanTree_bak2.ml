type tree = Char of int | Node of tree*tree ;;

let taille_alphabet = 256;;

(*Fonctions et types auxiliaires*)
type forest = (int*tree) list;;

let creer_foret_pas_triee (tab : int array) : forest = 
        let foret = ref [] in
        for a=0 to taille_alphabet-1 do
                let f_a = tab.(a) in
                if f_a > 0 then
                        foret := (f_a, Char a)::!foret;
        done;
        !foret
;;

let compare_int_arbre (f_x, _) (f_y, _) = 
        if f_x = f_y 
        then 0
        else 
                if f_x > f_y 
                then 1
                else -1
;;

let trie_foret (f: forest) : forest = 
        List.sort compare_int_arbre f
;;

let creer_foret_triee (tab : int array) : forest = 
        let f = creer_foret_pas_triee tab in
        trie_foret f
;;

let contient_au_moins_deux_arbres (f:forest) : bool = 
        (List.length f) >= 2
;;

let extraire_arbre_freq_minimale (f:forest) : ((int*tree) *forest) = 
        let (fa, a) = List.hd f in
        let f2 = List.tl f in
        ((fa, a), f2)
;;

(*Fonctions principales*)
let build_tree tab : tree = 
        let foret  = ref (creer_foret_triee tab) in
        while contient_au_moins_deux_arbres !foret do
                let (fA1, a1), foret2 = extraire_arbre_freq_minimale !foret in
                foret := foret2;
                let (fA2, a2), foret3 = extraire_arbre_freq_minimale !foret in
                foret := foret3;
                let fn = fA1 + fA2 in
                let n = Node(a1, a2) in
                foret := (fn, n) ::!foret;
                foret := trie_foret !foret;
        done;
        let fA, a = List.hd !foret in
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
        |0 -> Node(input_tree inp, input_tree inp)
        |_ -> failwith "Fin de l'arbre prématurée"
;;

