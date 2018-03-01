let my_subset_test0 = subset [2;1;2] [1;2;3]
let my_subset_test1 = not (subset [1;2;3] [4;5;6])

let my_equal_sets_test0 = equal_sets [1;3] [3;1;3]
let my_equal_sets_test1 = not (equal_sets [1;3] [3;1;4])
let my_equal_sets_test2 = not (equal_sets [1;3;4] [3;1;3])


let my_set_union_test0 = equal_sets (set_union [4;5;6] [1;2;3]) [1;2;3;4;5;6]

let my_set_intersection_test0 =
  equal_sets (set_intersection [1] [1;2;3]) [1]
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;1;2] [1;2;3]) [1;2;3]

let my_set_diff_test0 = equal_sets (set_diff [1] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [8;2;3;1;1;3] [1;3]) [8;2]
let set_diff_test2 = equal_sets (set_diff [2] [4;3;1]) [2]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x mod 5) 75 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 50. = infinity

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x / 2) 0 (+4) = 4

let my_while_away_test0 = while_away ((+) 2) ((>) 10) 2 = [2; 4; 6; 8]

let my_rle_decode_test0 = rle_decode [1,9; 2,1] = [9; 1; 1]
let my_rle_decode_test1 = rle_decode [1,"a"; 2,"b"; 3,"c"] = ["a"; "b"; "b"; "c"; "c"; "c"]

type awksub_nonterminals =
  | Honda | Toyota | Lexus | BMW | PC

let awksub_rules =
   [Honda, [T"("; N Honda; T")"];
    Honda, [N PC];
    Honda, [N Honda; N BMW; N Honda];
    Honda, [N Lexus; N Toyota];
    Honda, [N Toyota; N Lexus];
    Toyota, [T"$"; N Honda];
    Lexus, [T"++"];
    Lexus, [T"--"; N Toyota];
    BMW, [T"+"];
    BMW, [T"-"];
    PC, [T"Asus"];
    PC, [T"Sony"];
    PC, [T"Mac"];
    PC, [T"Samsung"];
    PC, [T"Alienware"];
    PC, [T"Dell"]]

let awksub_grammar = Honda, awksub_rules
let my_awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar

 let my_awksub_test1 =
  filter_blind_alleys (Honda, List.tl awksub_rules) = (Honda, List.tl awksub_rules)

 let my_awksub_test2 =
  filter_blind_alleys (Honda, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Honda, List.tl (List.tl awksub_rules))

type giant_nonterminals =
  | Iphone | Nokia | Samsung | Blackberry | Oppo 

let giant_grammar =
  Iphone,
  [Blackberry, [T"dieAlready"];
   Oppo, [];
   Samsung, [T"stillOkay"];
   Nokia, [T"strongOne!"];
   Iphone, [N Samsung];
   Iphone, [N Blackberry];
   Samsung, [N Iphone];
   Oppo, [N Iphone];
   Oppo, [N Iphone; T","; N Blackberry]]


let giant_test0 =
  filter_blind_alleys giant_grammar = giant_grammar

let giant_test1 =
filter_blind_alleys (Nokia, List.tl (snd giant_grammar)) =
(Nokia,
 [(Oppo, []); (Samsung, [T "stillOkay"]); (Nokia, [T "strongOne!"]);
  (Iphone, [N Samsung]); (Samsung, [N Iphone]); (Oppo, [N Iphone])])

;;



