let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

(* An example grammar for a small subset of Awk, derived from but not
   identical to the grammar in
   <http://web.cs.ucla.edu/classes/winter06/cs132/hw/hw1.html>.
   Note that this grammar is not the same as Homework 1; it is
   instead the same as the grammar under "Theoretical background"
   above.  *)

type awksub_nonterminals =
  | School | Term | Quality | Tobe | Name

let awkish_grammar =
  (School,
   function
     | School ->
         [[N Term; N Tobe; N School];
          [N Term]]
     | Term ->
         [[N Name];
         [T"("; N School; T")"]]
     | Quality ->
         [[T"better"; N School]]
     | Tobe ->
         [[T"is"];
         [T"are"];
         [T"than"]]
     | Name ->
         [[T"USC"]; [T"UCLA"]; [T"UCB"]; [T"UCSD"]; [T"Stanford"]])


let test_1 =
  ((parse_prefix awkish_grammar accept_all ["boom"])
   = None)

let test_2 =
  ((parse_prefix awkish_grammar accept_all ["UCLA"; "is"; "better"; "than"; "USC"])
   = Some
 ([(School, [N Term]); (Term, [N Name]); (Name, [T "UCLA"])],
  ["is"; "better"; "than"; "USC"]))


