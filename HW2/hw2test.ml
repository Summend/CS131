type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let test_convert_grammar_1 = ((snd (convert_grammar(Conversation, giant_grammar))) Snore = [[T"ZZZ"]])
let test_convert_grammar_2 = ((snd (convert_grammar(Conversation, giant_grammar))) Sentence = [[N Quiet];[N Grunt];[N Shout]])
let test_convert_grammar_3 = ((snd (convert_grammar(Conversation, giant_grammar))) Conversation = [[N Snore];[N Sentence; T","; N Conversation]])

let accept_all derivation string = Some (derivation, string)

type if_lan_nonterminals = Statement | IF_statement | Expression

let if_lan_grammar = (IF_statement,function
| Statement -> [[T "s1"]; [T "s2"]; [N IF_statement]]
| IF_statement -> [[T "if"; N Expression; T "then"; N Statement; T "else"; N Statement];
[T "if"; N Expression; T "then"; N Statement]]
| Expression -> [[T "e1"];[T "e2"]])

let test_1 = ((parse_prefix if_lan_grammar accept_all ["if";"e1";"then";"if";"e2";"then";"s1";"else";"s2"])
	= Some
 ([(IF_statement,
    [T "if"; N Expression; T "then"; N Statement; T "else"; N Statement]);
   (Expression, [T "e1"]); (Statement, [N IF_statement]);
   (IF_statement, [T "if"; N Expression; T "then"; N Statement]);
   (Expression, [T "e2"]); (Statement, [T "s1"]); (Statement, [T "s2"])],
  []))

type english_nonterminals = Sentence | DP | NP | VP

let english_grammar = (Sentence,function
| Sentence -> [[N DP; N VP]]
| DP -> [[T "I"]; [T "an"; N NP]]
| VP -> [[T "eat"; N DP]]
| NP -> [[T "apple"]])

let test_2 = ((parse_prefix english_grammar accept_all ["I";"eat";"an";"apple"])
	= Some
 ([(Sentence, [N DP; N VP]); (DP, [T "I"]); (VP, [T "eat"; N DP]);
   (DP, [T "an"; N NP]); (NP, [T "apple"])],
  []))

