let rec exists x s = match s with
  | [] -> false
  | h::t -> if x = h then true else exists x t;;

let rec subset a b = match a with
  | [] -> true
  | h :: t -> if exists h b then subset t b else false;;

let equal_sets a b =
  if subset a b && subset b a then true else false;;

let set_union a b = a @ b;;

let rec set_intersection a b = match a with
  | [] -> []
  | h :: t -> if exists h b then h :: set_intersection t b else set_intersection t b;;

let rec set_diff a b = match a with
  | [] -> []
  | h :: t -> if exists h b then set_diff t b else h :: set_diff t b;;

let rec computed_fixed_point eq f x =
  if eq (f x) x then x else computed_fixed_point eq f (f x);;

(* helper function for computed_periodic point, return the value after p f x calls *)
let rec f_pcalls f p x =
  if p = 0 then x else f_pcalls f (p-1) (f x);;

let rec computed_periodic_point eq f p x =
  if eq (f_pcalls f p x) x then x else computed_periodic_point eq f p (f x);;

let rec while_away s p x =
  if p x then x :: while_away s p (s x) else [];;

(* helper function for rle_decode, return the rle foramt of one pair *)
let rec rl_decode_pair p = match p with
  | (0,x) -> []
  | (n,x) -> x :: rl_decode_pair (n-1,x);;

let rec rle_decode lp = match lp with
  | [] -> []
  | h :: t -> rl_decode_pair h @ rle_decode t;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec check_rule_rhs rhs = match rhs with
  | [] -> true
  | h :: t -> match h with
              | (T a) -> check_rule_rhs t
              | (N a) -> false;;

(* The following 3 functions help get the terminal rule list, nonterminal rule list, and terminal rule lhs list*)
let rec get_t_rule_list rl = match rl with
  | [] -> []
  | (a,b) :: t -> match check_rule_rhs b with
              | false -> get_t_rule_list t
              | true -> get_t_rule_list t @ [(a,b)];;

let rec get_nt_rule_list rl = match rl with
  | [] -> []
  | (a,b) :: t -> match check_rule_rhs b with
          |true -> get_nt_rule_list t
          |false -> get_nt_rule_list t @ [(a,b)];;

let rec get_t_rule_lhs rl = match get_t_rule_list rl with
  | [] -> []
  | (a,b) :: t -> match check_rule_rhs b with
                  | true -> get_t_rule_lhs t @ [a]
                  | false -> get_t_rule_lhs t;;

(* helper function to check if a nonterminal rule can be parsed into terminal tokens*)
let rec check_nt_rule rule t_r_lhs = match rule with
  | (a,b) -> match b with
                  | [] -> true
                  | h :: t -> match h with
                              | (N s) -> if subset [s] t_r_lhs then check_nt_rule (a,t) t_r_lhs else false
                              | (T s) -> check_nt_rule(a,t) t_r_lhs;;

(*helper function to update the terminal rule list and terminal rule lhs list *)
let rec new_t_rules nt_rules t_rules t_r_lhs =
  match nt_rules with
  | [] -> t_rules
  | (a,b) :: t -> if (check_nt_rule (a,b) t_r_lhs && (subset [(a,b)] t_rules = false))
                  then new_t_rules t (t_rules @ [(a,b)]) t_r_lhs
                else new_t_rules t t_rules t_r_lhs;;

let rec new_t_rules_lhs nt_rules t_r_lhs =
  match nt_rules with
  | [] -> t_r_lhs
  | (a,b) :: t -> if (check_nt_rule (a,b) t_r_lhs && (subset [a] t_r_lhs = false))
                  then new_t_rules_lhs t (t_r_lhs @ [a])
                else new_t_rules_lhs t t_r_lhs;;

let rec filter_pass nt_rules t_rules t_r_lhs =
  if (List.length t_rules = List.length (new_t_rules nt_rules t_rules t_r_lhs))
  then t_rules
else (filter_pass nt_rules (new_t_rules nt_rules t_rules t_r_lhs) (new_t_rules_lhs nt_rules t_r_lhs));;

(* helper function to set the order of output *)
let rec set_order rules t_rules = match rules with
  | [] -> []
  | h :: t -> if subset [h] t_rules then [h] @ set_order t t_rules else set_order t t_rules;;

let filter_blind_alleys g = match g with
  | (a,b) -> (a, set_order b (filter_pass b [] []));;
