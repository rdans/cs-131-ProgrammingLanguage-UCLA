(* Study source + the helper function source : 
https://www.youtube.com/watch?v=tHldz6L19Pc *)

(* Define symbols for nonterminal and terminal *)
type ('nonterminal, 'terminal) symbol = 
	| N of 'nonterminal
	| T of 'terminal 

(* Number 1 *)
let rec in_list a b = match b with
	| [] -> false
	| h::t -> if h = a then true 
		else in_list a t 

let rec subset a b = match a with
	| [] -> true
	| h::t -> if in_list h b then subset t b 
		else false  

(* Number 2 *)
let rec equal_sets a b = (subset a b) && (subset b a) 

(* Number 3 *)
let rec set_union a b = match a with
	| [] -> b 
	| h::t -> if (in_list h b) then (set_union t b)
		else h::(set_union t b)   

(* Number 4 *)
let rec set_intersection a b = match a with 
	| [] -> []
	| h::t -> if (in_list h b) then h::(set_intersection t b)
		else set_intersection t b

(* Number 5 *)
let set_diff a b = List.filter (fun x -> not (in_list x b)) a

(* Number 6 *)
let rec computed_fixed_point eq f x =
	if eq x (f x) then x else computed_fixed_point eq f (f x)

(* Number 7 *)
let rec computed_periodic_point eq f p x = match p with
	| 0 -> x
	| _ -> if (eq (f(computed_periodic_point eq f (p-1) (f x))) x) then x
			else computed_periodic_point eq f p (f x)

(* Number 8 *)
let rec while_away s p x =
	if (p x) then x::(while_away s p (s x))
		else []

(* Number 9 *)
let rec rle_decode lp = match lp with
	| [] -> []
	| (n, e)::t -> if n != 0 then e::rle_decode ((n-1, e)::t)
		else rle_decode(t) 

(* Number 10 *)

let subset_check safe_rule = function
	| N sym -> subset [sym] safe_rule;
	| T sym -> true

let rec t_and_non_t safe_rule = function
	| [] -> true
	| h::t -> if (subset_check safe_rule h) then t_and_non_t safe_rule t 
		else false

let rec safe_list safe_rule = function
	| [] -> safe_rule
	| (a, b)::t -> if (t_and_non_t safe_rule b)
		then (
			if (subset [a] safe_rule) then safe_list safe_rule t 
				else safe_list (a::safe_rule) t)
		else safe_list safe_rule t

let fixed_point_helper (safe_rule, rule) =
	((safe_list safe_rule rule), rule)

let rules_counter (safe_rule, rule) =  
	fst(computed_fixed_point (fun (a, _) (b, _) -> equal_sets a b) fixed_point_helper ([], rule))

let rec incr_list safe_rule = function
	| [] -> []
	| (a, b)::t -> if (t_and_non_t safe_rule b) then (a, b)::(incr_list safe_rule t) 
		else incr_list safe_rule t

let filter_blind_alleys  = function
	| (start, rule) -> (start, incr_list (rules_counter ([], rule)) rule) 

;;
	

