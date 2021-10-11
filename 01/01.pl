:- op(800, xfy, =>).

get(Key, Assoc, Value) :- get_assoc(Key, Assoc, Value), !.
get(_, _, 0).

put(Key, Assoc, Value, NewAssoc) :- put_assoc(Key, Assoc, Value, NewAssoc).

%% wyrażenia arytmetyczne

(nat(N), _) => N :- !.
(var(X), State) => N :- !, get(X, State, N).
(add(A1, A2), State) => N :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N is N1 + N2.
(sub(A1, A2), State) => N :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N is N1 - N2.
(mul(A1, A2), State) => N :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N is N1 * N2.

%% wyrażenia boolowskie

(true, _) => true :- !.
(false, _) => false :- !.
(eq(A1, A2), State) => true :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N1 = N2.
(eq(A1, A2), State) => false :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N1 \= N2.
(leq(A1, A2), State) => true :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N1 =< N2.
(leq(A1, A2), State) => false :- !,
    (A1, State) => N1,
    (A2, State) => N2,
    N1 > N2.
(not(B), State) => false :- (B, State) => true, !.
(not(B), State) => true :- (B, State) => false, !.
(and(B1, _), State) => false :- (B1, State) => false, !.
(and(B1, B2), State) => B3 :-
    (B1, State) => true, !,
    (B2, State) => B3.
(or(B1, _), State) => true :- (B1, State) => true, !.
(or(B1, B2), State) => B3 :-
    (B1, State) => false, !,
    (B2, State) => B3.

%% instrukcje

(skip, State) => State :- !.
(assign(var(X), A), State1) => State2 :- !,
    (A, State1) => N,
    put(X, State1, N, State2).
(seq(C1, C2), State1) => State3 :- !,
    (C1, State1) => State2,
    (C2, State2) => State3.
(if(B, C1, _), State1) => State2 :-
    (B, State1) => true, !,
    (C1, State1) => State2.
(if(B, _, C2), State1) => State2 :-
    (B, State1) => false, !,
    (C2, State1) => State2.
(while(B, _), State) => State :-
    (B, State) => false, !.
(while(B, C), State1) => State3 :-
    (B, State1) => true, !,
    (C, State1) => State2,
    (while(B, C), State2) => State3.







