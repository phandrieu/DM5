(** Correction DM 05 : compression de Huffman
    MP2I au Lycée Kléber, Lilian Besson
    Avril 2022
    Merci à Marc de Falco, MP2I au CIV.
*)

(** Fichiers binaires en lecture *)
type in_channel_bits = {
    i_file : in_channel;
    mutable i_acc : int;
    mutable i_acc_bits : int;
    i_length : int;
};;

(** Création d'un fichier binaire en lecture f_in depuis un fichier de nom fn *)
let open_in_bits (fn : string) : in_channel_bits =
    let file_in = open_in_bin fn in
    {
        i_file = file_in;
        i_acc = 0;
        i_acc_bits = 0;
        i_length = in_channel_length file_in;
    }
;;

(** Lecture d'un seul bit depuis le fichier binaire en lecture f_in *)
let input_bit (f_in : in_channel_bits) : bool =
    if f_in.i_acc_bits = 0 then
    begin
        f_in.i_acc <- input_byte f_in.i_file;
        f_in.i_acc_bits <- 8;
        if pos_in f_in.i_file = f_in.i_length - 1 then
        begin
            let pad = input_byte f_in.i_file in
            f_in.i_acc_bits <- f_in.i_acc_bits - pad
        end
    end;
    let bit = ((f_in.i_acc mod 2) = 1) in
    f_in.i_acc <- f_in.i_acc / 2;
    f_in.i_acc_bits <- f_in.i_acc_bits - 1;
    bit
;;

(** Fermeture d'un fichier binaire en lecture f_in *)
let close_in_bits (f_in : in_channel_bits) : unit =
    close_in f_in.i_file
;;

(** Un itérateur, comme List.iter ou Array.iter, qui applique la fonction func_iter (byte=int -> unit) sur chaque bytes c du fichier binaire en lecture f_in obtenu depuis le fichier de nom fn *)
let input_byte_iter (func_to_iter : int -> unit) (fn : string) : unit =
    let f_in = open_in_bin fn in
    begin
        try
            while true do
                let c = input_byte f_in in
                func_to_iter c
            done
        with End_of_file -> ()
    end;
    close_in f_in
;;

(** Fichiers binaires en écriture *)
type out_channel_bits = {
    o_file : out_channel;
    mutable o_acc : int;
    mutable o_acc_bits : int;
};;

(** Création d'un fichier binaire en écriture, depuis fn un nom de fichier *)
let open_out_bits (fn : string) : out_channel_bits =
    {
        o_file = open_out_bin fn;
        o_acc = 0;
        o_acc_bits = 0;
    }
;;

(** Écriture d'un bit (un booléen) dans le fichier binaire en écriture f_out *)
let output_bit (f_out : out_channel_bits) (b : bool) : unit =
    if f_out.o_acc_bits = 8 then
    begin
        output_byte f_out.o_file f_out.o_acc;
        f_out.o_acc <- 0;
        f_out.o_acc_bits <- 0
    end;
    if b then
        f_out.o_acc <- f_out.o_acc + (1 lsl f_out.o_acc_bits);
    f_out.o_acc_bits <- 1 + f_out.o_acc_bits
;;

(* Écriture manuelle de la fonction Int.shift_left qui est dans le module Int, nouveau depuis la version OCaml 4.08
    Cf. https://ocaml.org/api/Int.html#VALshift_left et https://github.com/ocaml/ocaml/search?p=3&q=shift_left
    Le code n'est pas du pure OCaml c'est du C, dans le code de `Int.ml` de la lib standard de OCaml.
    Mais cette fonction fait juste un décalage vers la gauche de y bits, donc il suffit de d'abord calculer 2^y (avec 1 lsl y) et de multiplier x par 2^y.
*)
let shift_left x y =
    x * (1 lsl y)
;;

(** Fermeture du fichier binaire en écriture f_out *)
let close_out_bits (f_out : out_channel_bits) : unit =
    if f_out.o_acc_bits = 0 then
        output_byte f_out.o_file 0
    else begin
        let padding = 8 - f_out.o_acc_bits in
        output_byte f_out.o_file (shift_left f_out.o_acc padding);
        output_byte f_out.o_file padding
    end;
    close_out f_out.o_file
;;

(** Lecture d'un byte (un entier entre 0 et 255, un octet) depuis le fichier binaire d'entrée f_in *)
let input_byte (f_in : in_channel_bits) : int =
    input_byte f_in.i_file
;;

(** Écriture d'un byte dans un fichier binaire en écriture f_out *)
let output_byte (f_out : out_channel_bits) (byte : int) : unit =
    output_byte f_out.o_file byte
;;
