/* Fact to translate signal to morse code */
translate([1], '.').
translate([1,1], '.').
translate([1,1|_], '-').
translate([0], '0').
translate([0,0], '0').
translate([0,0], '^').
translate([0,0,0], '^').
translate([0,0,0,0], '^').
translate([0,0,0,0,0], '^').
translate([0,0,0,0,0|_], '#').

/* recursive function to iterate the signal into morse*/
helper([], S, [M_head]) :- translate(S, M_head).
helper([B_head|B_tail], [], M) :- helper(B_tail, [B_head], M).
helper([B_head|B_tail], [S_head|S_tail], M) :-
		S_head = B_head,
        helper(B_tail,[B_head,S_head|S_tail],M).
helper(B, S, [M_head|M_tail]) :-
        S = [S_head|_],
        B = [B_head|_],
        B_head \= S_head,
        translate(S, M_head),
	    helper(B,[],M_tail).

removezero([],[]).
removezero(['0'|M_tail],Rest) :- removezero(M_tail,Rest),!.
removezero([M_head|M_tail],[M_head|Rest]) :- removezero(M_tail,Rest).

/* predicate signal_morse that converts a list of 1s and 0s to */
/* the corresponding list of .s, -s, ^s, and #s. */
signal_morse(B,Rest) :- helper(B, [], M), removezero(M, Rest).

/*---------------PART 2---------------------*/

/* Morse code table, derived from Recommendation ITU-R M.1677-1 (2009) */
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

/* convert morse into char */
characters([], S, [C]) :- morse(C, S).
characters(['^'|M_tail], S, [C|X]) :- morse(C, S), characters(M_tail, [], X).
characters(['#'|M_tail], S, [C,'#'|X]) :- morse(C, S), characters(M_tail, [], X).
characters([M_head|M_tail], S, X) :- append(S,[M_head], Sn), characters(M_tail, Sn, X).

/* error handler */
errorhandler([], W, W).
errorhandler(['error'|Err_tail], [], ['error'|X]) :- errorhandler(Err_tail, [], X),!.
errorhandler(['error'|Err_tail], _, X) :- errorhandler(Err_tail, [], X),!.
errorhandler(['#'|Err_tail], W, Xn) :- errorhandler(Err_tail, [], X), append(W, ['#'|X], Xn),!.
errorhandler([Err_head|Err_tail], W, X) :- append(W,[Err_head],Wn), errorhandler(Err_tail, Wn, X).

/* implementation of signal_message with combination of predicates*/
signal_message(B,X) :- signal_morse(B,M), characters(M, [], Err), errorhandler(Err, [], X).
