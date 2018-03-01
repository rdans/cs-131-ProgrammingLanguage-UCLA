(* Warm up problem. Converting the rule*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec list_of_rules non_t rules = match rules with
	| [] -> []
	| (expr, rest)::t -> if expr = non_t then 
							rest::(list_of_rules non_t t) 
						else
							list_of_rules non_t t

let convert_grammar gram1 = match gram1 with 
	 (start, rules) -> (start, fun non_t -> (list_of_rules non_t rules))

(*Parsing start here*)

let parse_prefix gram =
	
	let sec_component = (snd gram)
	in

	let rec nont_match sec_component non_tsym = function
		| [] -> (fun acceptor derivation frag -> None)
		| head::tail -> (fun acceptor derivation frag ->
			let rest_matcher = nont_match sec_component non_tsym tail and 
			list_matcher = t_match sec_component head
			in
			let or_matcher = list_matcher acceptor (derivation @ [(non_tsym, head)]) frag
			in match or_matcher with
				| None -> rest_matcher acceptor derivation frag
				| _ -> or_matcher)

	and t_match sec_component = function
		| [] -> (fun acceptor derivation frag -> acceptor derivation frag)
		(* match terminal symbol*)
		| (T t_sym)::tail -> (fun acceptor derivation -> function
				| [] -> None
				| fragment_h::fragment_t ->
					let matcher = t_match sec_component tail
					in 
						if fragment_h = t_sym then 
							matcher acceptor  derivation fragment_t
						else
							None
			)
		(* match non terminal symbol *)
		| (N non_tsym)::tail -> (fun acceptor derivation frag ->
				let rest_matcher = nont_match sec_component non_tsym (sec_component non_tsym)
				and acceptor_tmp = t_match sec_component tail acceptor 
				in rest_matcher acceptor_tmp derivation frag)
	in
	fun acceptor frag -> nont_match sec_component (fst gram) (sec_component (fst gram)) acceptor [] frag
;;
