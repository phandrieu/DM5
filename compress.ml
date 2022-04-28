open FileIO;;
open HuffmanTree;;

let stats (fn:string) : int array = 
        let tableau_frequences = Array.make 256 0 in
        FileIO.input_byte_iter (fun n -> tableau_frequences.(n) <- tableau_frequences.(n) +1) fn;
        tableau_frequences
;;

let output_code (out : FileIO.out_channel_bits) (l : bool list) =
        List.iter (fun b -> output_bit out b) l
;;

(*Pour le debug : *)
let print_codes (codes : bool list array) =
        let n = Array.length codes in
      
        let rec print_list (liste : bool list) =
          match liste with
          | [] -> ()
          | h :: t when h -> ( Printf.printf "%i" 1; print_list t; )
          | h :: t when (not h) -> ( Printf.printf "%i" 0; print_list t; )
          | _ :: _ -> failwith "Impossible"
        in
        
        for i = 0 to (n-1) do
          if codes.(i) <> [] then begin
            (Printf.printf "%i\t%c\t" i (Char.chr i));
            print_list codes.(i);
            Printf.printf "\n\n";
            end;
        done
      ;;


let compress (fn : string) =
        let tab = stats fn in
        let tree = HuffmanTree.build_tree tab in
        let codes = HuffmanTree.codes tree in
        print_codes codes;
        let out = FileIO.open_out_bits (fn ^ ".huffman") in
        HuffmanTree.output_tree out tree;
        FileIO.input_byte_iter (fun x -> output_code out (codes.(x))) fn;
        close_out_bits out
;;
