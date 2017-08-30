let my_subset_test0 = subset [1;2;3] [1;2;3;4]
let my_subset_test1 = subset [2] [1;2;3]
let my_subset_test2 = not (subset [1;3;3;3;7] [1;3;3;3])

let my_equal_sets_test0 = equal_sets [1;1;4] [1;1;1;4]
let my_eqaul_sets_test1 = not (equal_sets [1;3;3;5] [1;1])

let my_set_union_test0 = equal_sets (set_union [1;2;3] [1;3;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]

let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3] [1]) [1]
let my_set_intersection_test1 = equal_sets (set_intersection [3;4;5] [6;7;8]) []

let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [1]) [2;3]
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [1;2;3]) []
let my_set_diff_test2 = equal_sets (set_diff [2;3;4;5] [6;7;8]) [2;3;4;5]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x * x) 1 = 1
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x * x / 2) 2 = 2

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x * x) 0 (1) = 1
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> x * x * x) 1 (-1) = -1

let my_while_away_test0 = while_away ((+) 2) ((>) 5) 0 = [0;2;4]
let my_while_away_test1 = while_away ((-) 2) ((>) 5) 10 = []
let my_while_away_test2 = while_away ((-) 2) ((<) 5) 10 = [10]

let my_rle_decode_test0 = rle_decode [1,2;2,5;3,7;0,8] = [2; 5; 5; 7; 7; 7]
let my_rle_decode_test1 = rle_decode [1,"w";2,"aa";0,"B"] = ["w"; "aa"; "aa"]

(* test cases for blind-alley rules *)
type my_nonterminals =
  | Expr | Rvalue | Binop | Sym

let my_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Sym];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Rvalue];
    Rvalue, [T"$"; N Sym];
    Rvalue, [N Sym; N Expr];
    Binop, [T"+"];
    Binop, [T"-"];
    Sym, [T"A"];
    Sym, [T"B"];
    Sym, [T"C"];
    Sym, [T"D"]]

let my_grammar = Expr, my_rules

let my_filter_blind_alleys_test0 = filter_blind_alleys my_grammar = my_grammar
let my_filter_blind_alleys_test1 = filter_blind_alleys (Expr,
[Expr, [N Expr]; Expr,[N Expr; N Rvalue]; Rvalue, [N Rvalue]; Sym, [T"A"]]) = (Expr, [(Sym, [T "A"])])

let my_filter_blind_alleys_test2 = filter_blind_alleys (Expr,
[Expr, [N Expr]; Expr,[N Expr; N Rvalue]; Binop, [T"+"]; Sym, [T"A"]]) = (Expr, [(Binop, [T "+"]); (Sym, [T "A"])])


