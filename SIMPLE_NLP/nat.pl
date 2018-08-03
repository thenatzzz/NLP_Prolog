%%%%% Natural Language Program

sentence(S) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP, VP, S).

noun_phrase(NP) :-
	article(A),
	noun(N),
	append(A, N, NP).

verb_phrase(V) :-
	verb(V).
verb_phrase(VP) :-
	verb(V),
	noun_phrase(NP),
	append(V, NP, VP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* The required predicates and argument positions are:
    
	a.  conj(Text)
	b.  encode(Text, EncodedText)
	c.  same_actor(Text)
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1.
conj(C):- sentence(C).

conj(C):- sentence(S),
	  conjunc(Conjunc),
	  append(S,Conjunc,S2),
	  append(S2,C2,C),
	  conj(C2).


conjunc([and]).
conjunc([or]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2.

% base case
encode([],[]). 

% recursive case when Head of a list is noun
encode([H1|T1],[H2|T2]):- noun([H1]),
	                   atom_chars(H1,In),
	                   translate(In,Out),
	                   atom_chars(H2,Out),
	                   encode(T1,T2).
% recursive case when Head of a list is not noun
encode([H1|T1],[H1|T2]):- \+(noun([H1])),
                          encode(T1,T2).

% The translate predicate converts noun input into particular output by
% appending 3 letters which by themselves have their own specific predicates 
translate(Input,Output):- first_char(Input,_1stCharOutput),
	                  append([],_1stCharOutput,Output1),
	                  second_char(Input,_2ndCharOutput),
	                  append(Output1,_2ndCharOutput,Output2),
	                  third_char(Input,_3rdCharOutput),
	                  append(Output2,_3rdCharOutput,Output3),
	                  Output = Output3.

% This predicate 'first_char' states that if input is animate, output will be 'a' and
% if input is not animate, output will be 'd'.
first_char(_1stCharInput,[a]):- atom_chars(List,_1stCharInput),
	           		animate(List_animal),
	           		member(List,List_animal).
first_char(_1stCharInput,[d]):- \+(first_char(_1stCharInput,[a])).

% This predicate 'second_char' states that if input has length more than 3, output will be 'l' and
% if input has length equal or less than 3, output will be 's'.
second_char(_2ndCharInput,[l]):- length(_2ndCharInput,Length),
	            		 Length > 3.
second_char(_2ndCharInput,[s]):- length(_2ndCharInput,Length),
	                         Length =< 3.

% This predicate 'third_char' states that output will be the first letter of input. 
third_char([H|T],[H]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%3.

% Predicate 'acc_same_actor' acts as accumulator which keeps tracking what current noun phrase is.
same_actor(One_sentence):- acc_same_actor(One_sentence,Noun_tracker).

% Base case when there is only one sentence.
acc_same_actor(One_sentence,Noun_tracker):- sentence(One_sentence),
	                                    append(Actor,V,One_sentence),
	                                    noun_phrase(Actor),
	                                    Noun_tracker = Actor.

% Recursive case
acc_same_actor(Sentence,Noun_tracker):- append(Next_sentence,[and|R],Sentence),
	             			acc_same_actor(R,Noun_tracker),
                                        sentence(Next_sentence),
	                                append(Actor,_,Sentence),
	                                noun_phrase(Actor),
	                                Actor = Noun_tracker.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

