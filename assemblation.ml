open Format
open X86_64
open Asyntax

let entete = inline ("\t.globl main\n" ^ "\n" ^ "main:\n")

let print_int = inline ("\nprint_int:\n" ^"\tpushq %rbp\n" ^ "\tmov %rdi, %rsi\n" ^ "\tmov $message, %rdi\n" ^ "\tmov $0, %rax\n" ^ "\tcall printf\n" ^ "\tpopq %rbp\n" ^ "\tret\n")
let print_float = inline ("\nprint_float:\n\tmovq $nombre, %rdi\n"^"\tmovq $1, %rax\n"^"\tcall printf\n"^"\tret\n")

let pied_de_page t =match t with (*écrit le pied de page en en prenant en compte le type de la sortie (entier ou flottant)*)
|"int" -> movq !%rax !%rdi ++ inline ("\tcall print_int\n") ++ movq (imm 0) !%rax ++ ret
|"float" -> (*movsd xmm0 !%rdi ++*) inline ("\tcall print_float\n") ++ movq ( imm 0) !%rax ++ ret
|_ -> raise(Error "Invalid arguments for pied_de_page t ")


let mess = inline ("message:\n\t.string \"%d\\n\"\n" ^ "nombre:\n\t.string \"%f\\n\"\n")


let ds =  {texte = "" ; num = 0}  (*permet d'ajouter des flottants au code*)
 

let int_appl_recon s = (*fais le matching comme il faut t'as capté*)
        match s with
        |"+" -> nop
        |"-" -> negq !%rax
        |"float" ->  cvtsi2sdq !%rax xmm0
        |_-> raise (Error "cette erreur n'arrivera pas mais comme ça vscode est content")

let float_appl_recon s = (*fais le matching comme il faut t'as capté*)
        match s with
        |"+" -> nop        
        |"-" -> mulsd (immf (-. 1.0) ds) xmm0
        |"int" -> cvttsd2siq !%xmm0 rax
        |_-> raise (Error "cette erreur n'arrivera pas mais comme ça vscode est content")
   
let int_op_recon s = (*idem*)
        match s with
        |"+" -> addq !%rbx !%rax
        |"-" -> subq !%rbx !%rax
        |"*" -> imulq !%rbx !%rax
        |"/" -> movq (imm 0) !%rdx ++ idivq !%rbx
        |"%" -> movq (imm 0) !%rdx ++ idivq !%rbx ++ movq !%rdx !%rax
        |_ -> raise (Error "cette erreur n'arrivera pas mais comme ça vscode est content")
        
let float_op_recon s =(*itou*)
        match s with
        |"+." -> addsd !%xmm1 xmm0
        |"-." -> subsd !%xmm1 xmm0
        |"*." -> mulsd !%xmm1 xmm0
        |_ -> raise (Error "cette erreur n'arrivera pas mais comme ça vscode est content")
        
let rec calculos tast  = (*s'occupe d'écrire le programme en assembleur (la partie intéressante)*)
        match tast with
        | Int(x) -> movq (imm x) !%rax
        | Float(x) -> movsd (immf x ds) xmm0 
        | Appl(s, tast2) when List.mem s l_intappl           -> calculos tast2  ++ int_appl_recon s
        | Appl(s, tast2) when List.mem s l_floatappl         -> calculos tast2  ++ float_appl_recon s 
        | Appl("()", tast2)                                  -> calculos tast2 
        | Op(s, tast2, tast3) when List.mem s l_intop        -> calculos tast3  ++ pushq !%rax ++ calculos tast2  ++ popq rbx ++ int_op_recon s
        | Op(s, tast2, tast3)                                -> calculos tast3  ++ pushsd !%xmm0 ++ calculos tast2  ++ popsd xmm1 ++ float_op_recon s
        | _ -> raise (Error "int_calc")

let serge_haddad tast t = (*je ne sais plus comment appeler mes fonction*) (*Grosso modo assemnle (mdr) les différents morceaux du programme*)
        let prog = calculos tast in
        { text = entete ++ prog ++ (pied_de_page t) ++ print_int ++  inline "\n" ++ print_float ++ inline (ds.texte ^"\n");
                data = mess;}