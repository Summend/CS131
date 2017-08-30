type ('n, 't) symbol = N of 'n | T of 't

let rec rule_finder rules lhs = match rules with
| [] -> []
| (a,b)::t -> (if (a = lhs) then b :: (rule_finder t lhs)
				else rule_finder t lhs);;

let convert_grammar gram1 = match gram1 with
| (start, rules) -> (start, (fun x -> (rule_finder rules x)));;

let rec matcher_and rule_list rule acceptor drv frag = match rule with
| [] -> acceptor drv frag
| _ -> match frag with
	| [] -> None
	| h::t -> match rule with
		| (T sym)::t_tail -> if (h = sym) then (matcher_and rule_list t_tail acceptor drv t) else None
		| (N sym2)::n_tail -> (matcher_or sym2 rule_list (rule_list sym2) (matcher_and rule_list n_tail acceptor) drv frag)
		| _ -> None
and matcher_or start_sym rule_list rule_match acceptor drv frag = match rule_match with
| [] -> None
| h_rule::t_rule -> match (matcher_and rule_list h_rule acceptor (drv@[start_sym,h_rule]) frag) with
	| None -> (matcher_or start_sym rule_list t_rule acceptor drv frag)
	| x -> x

let parse_prefix grammar acceptor frag = match grammar with
| (start, rule_list) -> matcher_or start rule_list (rule_list start) acceptor [] frag;;
