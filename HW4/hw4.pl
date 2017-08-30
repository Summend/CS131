% think error as a word

signal_morse_reverse([],[]).
signal_morse_reverse(M,[H|T]):-interpreter(M,X,H), signal_morse_reverse(X,T).

signal_morse(L,M):-signal_morse_reverse(L,T),filter(T,M).

% filter takes out the * symbol used to separate dih from dah.
filter([],[]).
filter(['*'|T],M):-filter(T,M),!.
filter([H|T1],[H|T2]):-filter(T1,T2),!.

% L1 is the input list, R is the decoding result of first 
% consecutive items such as 1111 or 0000. L2 is the list of the
% remaining items.
interpreter(L1,L2,R):-front_sample(L1,G,L2), decode(G,R).

% decode is a matching relation specified in the course
% website. Use * to separate from dih and dah.
decode([1],'.').
decode([1,1],'.').
decode([1,1|_],'-').
decode([0],'*').
decode([0,0],'*').
decode([0,0],'^').
decode([0,0,0],'^').
decode([0,0,0,0],'^').
decode([0,0,0,0,0],'^').
decode([0,0,0,0,0|_],'#').

% front_sample gets the first consecutive items in the input
% list, such as 11111, 000, 00000.
front_sample([],[],[]).
front_sample([1|T1],[1|T2],R):-front_sample_1(T1,T2,R).
front_sample([0|T1],[0|T2],R):-front_sample_0(T1,T2,R).

front_sample_1([1|T1],[1|T2],R):-front_sample_1(T1,T2,R),!.
front_sample_1(T,[],T).

front_sample_0([0|T1],[0|T2],R):-front_sample_0(T1,T2,R),!.
front_sample_0(T,[],T).


morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)

signal_message(L,R):- signal_morse(L,M),letter_inter_2(M,F),error_finder(F,Q,S),
delete_word(S,G),append(G,Q,R),!.
signal_message(L,R):- signal_morse(L,M),letter_inter_2(M,R).

% letter interpreter
letter_inter_2([],[]).
letter_inter_2(L,R):- append(B,['#'|B1],L),letter_inter(B,M),letter_inter_2(B1,S),
append(M,['#'|S],R).
letter_inter_2(L,R):- letter_inter(L,R).
letter_inter([],[]).
letter_inter(L,[M|T]):- letter_inter_word(L,S,M),letter_inter(S,T).
letter_inter_word(L1,L2,R):- append(A,['^'|L2],L1),morse(R,A).
letter_inter_word(L1,[],R):- morse(R,L1).

% Find first error, L is the input, R is the list before error, S is after
error_finder(L,R,S):- reverse_list(L,RevL),append(N,['error'|M],RevL),reverse_list(M,S),
reverse_list(N,R),!.

% reverse the list
reverse_list([],[]).
reverse_list([H|T],R):- reverse(T,RevT),append(RevT,[H],R),!.

% delete the word before error
delete_word(L,R):- last_pound(L),reverse_list(L,RevL),delete_first_pound_word(RevL,S), 
reverse_list(S,R),!.

%delete the word before pound if it is the format of #error
delete_word(L,R):- reverse_list(L,RevL), append(B,['#'|B1],RevL), append(['#'],B1,S2), 
reverse_list(S2,R),!.

delete_first_pound_word(['#'|T],R):- append(B,['#'|B1],T), append(['#'],B1,R),!.
delete_first_pound_word(['#'|T],[]).

last_pound(['#']).
last_pound([_|Z]):- last_pound(Z),!.
