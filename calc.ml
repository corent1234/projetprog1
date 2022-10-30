(* File calc.ml *)
open Asyntax
open Assemblation
open X86_64
open String

let _ =
  let nom_fichier_ext = Sys.argv.(1) in (*extrait le nom du fichier avec l'extension*)
  let nom_fichier = String.sub nom_fichier_ext 0 (String.length nom_fichier_ext - 4) in (*puis sans l'extension*) 
  
  if not (nom_fichier ^ ".exp" = nom_fichier_ext)
  then raise (Error ".exp file expected" );

  let fichier= Arg.read_arg nom_fichier_ext in

  try 
    for i = 0 to Array.length fichier - 1 do 
      let lexbuf = Lexing.from_string (fichier.(i) ^"\n") in (* lit le fichier texte comme un grand*)
      let result = Parser.main Lexer.token lexbuf in (*affiche le tout ce gros bg*)
      
      let res = serge_haddad result (good_typing result) in (*récupère le programme assembleur *)
      print_in_file (nom_fichier ^".s") res; (*l'écrit*)
      
    done
  with End_of_file ->
    exit 0