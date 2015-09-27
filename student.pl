/*************************************************************
STUDENT de Bobrow
************************************************************/

% Carga de read_atom.pl

:- ensure_loaded('read_atom.pl').

%************************************************************

student:-
	read_atomics(Sentence),
	remove_noise(Sentence,Keywords),
	write(Keywords),nl,nl,
	translate_into_equation(Keywords,Equations),
	write(Equations),nl,
        solve_equations(Equations).
	

%************************************************************

% Elimina palabras que no sean resaltantes para el desarrollo del problema

remove_noise(List,Result) :-
	sr(List,NewList),
	!,
	remove_noise(NewList,Result).

remove_noise([W|Words],[W|NewWords]) :-
	remove_noise(Words,NewWords).

remove_noise([],[]).				% cuando ya no haya mas palabras para reducir

%************************************************************

sr([un|X],X).
sr([un|X],X).
sr([el|X],X).
sr([la|X],X).
sr([lo|X],X).
sr([este|X],X).
sr([esta|X],X).
sr([esto|X],X).
sr([ese|X],X).
sr([esa|X],X).
sr([eso|X],X).
sr([aquel|X],X).
sr([aquella|X],X).
sr([aquelllo|X],X).
sr([numero|X],X).
sr([de|X],X).
sr([del|X],X).
sr(['$'|X],X).
%sr([eres|X],[es|X],[son|X],[somos|X]).% 
sr([eres|X],[es|X]).%
sr([son|X],[es|X]).%
sr([somos|X],[es|X]).%
sr([eras|X],[es|X]).% 
sr([fui|X],[es|X]).%
sr([fue|X],[es|X]).%
sr([fuimos|X],[es|X]).
sr([fueron|X],[es|X]).
sr([soy|X],[es|X]).
sr([tengo|X],X).
sr([tiene|X],X).
sr([tienes|X],X).

 
%************************************************************

% Reglas para transformar el texto en un conjunto de ecuaciones en notacion prefija

% Primer conjunto de reglas: Estas dividen la oracion completa en oraciones independientes

rules(X,U)     :-   append(P1,['.'],X),rules(P1,U),!.
rules(X,[U,V]) :-   append(P1,['.'|P2],X),rules(P1,U),rules(P2,V),!
                    ;
                    append([si|P1],[',',entonces|P2],X),rules(P1,U),rules(P2,V),!
                    ;
                    append([si|P1],[','|P2],X),rules(P1,U),rules(P2,V),!
                    ;
                    append(P1,[',',y|P2],X),rules(P1,U),rules(P2,V),!.
 
% Segundo conjunto de reglas: Estas convierten partes de la oracion en una ecuacion

rules(X,[=,U,V]) :- append(P1,[igual|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[same,as|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[es,igual,a|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[=|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[es|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[-,U,V]) :- append(P1,[-|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[menos|P2],X),rules(P1,U),rules(P2,V),!.
                    
rules(X,[-,V,U]) :- append([diferencia,entre|P1],[y|P2],X),rules(P1,U),rules(P2,V),!;
                    append([diferencia|P1],[y|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[+,V,U]) :- append(P1,[+|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[mas|P2],X),rules(P1,U),rules(P2,V),!;
                    append([suma|P1],[y|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[*,V,U]) :- append(P1,[*|P2],X),rules(P1,U),rules(P2,V),!; 
                    append([producto|P1],[y|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[veces|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[/,U,V]) :- append(P1,[/|P2],X),rules(P1,U),rules(P2,V),!;
%                    append(P1,[por|P2],X),rules(P1,U),rules(P2,V),!;
%                    append(P1,[entre|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[dividido,por|P2],X),rules(P1,U),rules(P2,V),!;
                    append(P1,[dividido,entre|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[/,V,2]) :- [mitad|P2] = X, rules(P2,V),!;
                    [una,mitad|P2] = X, rules(P2,V),!. 

rules(X,[*,V,2]) :- [doble|P2] = X, rules(P2,V),!;
                    [dos,veces|P2] = X, rules(P2,V),!.

rules(X,[*,V,V]) :- [cuadrado|P2] = X, rules(P2,V),!.
 
rules(X,[*,V,[/,[-,100,U],100]]) :- append(P1,['%',menor,que|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[*,V,[/,[+,100,U],100]]) :- append(P1,['%',mayor,que|P2],X),rules(P1,U),rules(P2,V),!.

rules(X,[*,[/,U,100],V])         :- append(P1,['%'|P2],X),rules(P1,U),rules(P2,V),!.

rules([encuentre,X,y,Y],[[=, to_find_1, X],[=, to_find_2, Y]]).

rules([encuentre,X],[=,to_find,X]).

rules([X],X) :- number(X).
rules([X|_],X).


%************************************************************

translate_into_equation(Words,EquationsList):-
	rules(Words,Equations),
	write(Equations),nl,nl,
        create_list_of_equations(Equations,EquationsList).

%************************************************************

create_list_of_equations([H|T],L) :-
	create_list_of_equations(H,P),
	create_list_of_equations(T,S),
	append(P,S,L).

create_list_of_equations([H|T],[[H|T]]):-
	atom(H).

create_list_of_equations([],[]).

%************************************************************
% Ecuaciones

solve_equations(Equations):-
	print_equations('Las ecuaciones ha ser resueltas son: ',Equations),
	solve(Equations,Solutions),
	print_equations('La solucion es: ', Solutions).

%************************************************************

print_equations(S,E):-
	write(S),nl,
	print_aux(E).

print_aux([]).
print_aux([H|T]):-
	tab(5),write(H),nl,
	print_aux(T).

%************************************************************
% Operador inverso

inverse_op(+,-).
inverse_op(-,+).
inverse_op(*,/).
inverse_op(/,*).
inverse_op(=,=).

%************************************************************
% Operadores conmutativos

commutative(+).
commutative(*).
commutative(=).

%************************************************************

no_unknown([OP,P1,P2]):-
	no_unknown(P1),
	no_unknown(P2),
	OP \== '='.

no_unknown([OP,P1,P2]):-
	number(P1),
	number(P2),
	OP \== '='.

no_unknown(P):- number(P).

one_unknown([_,P1,P2],X) :-
	no_unknown(P1),
	one_unknown(P2,X),!. 

one_unknown([_,P1,P2],X) :-
	no_unknown(P2),
	one_unknown(P1,X),!. 

one_unknown(X,X):- atom(X),!.

%************************************************************
% El motor que permite la resolucion del conjunto de ecuaciones
% Esta basado en predicados aislados y algunas funciones auxiliares


solve(E,S) :-
	solve_aux(E,[],[],S).

solve_aux([E1|E],R,S,L) :-
	one_unknown(E1,X),
	!,
	isolate(X,E1,[X,V|_]),
	substitute(X,E,V,NT),
	substitute(X,R,V,NH),
	append(NH,NT,NE),
	solve_aux(NE,[],[[X,V]|S],L).

solve_aux([E1|E],R,S,L) :-
	solve_aux(E,[E1|R],S,L).

solve_aux([],_,S,S).

%************************************************************
% substitute(Subterm, Term, Subterm1, Term1)
% Si todas las ocurrencias de Subterm en Term son substituidos en
% Subterm1 se consigue Term1.

substitute(Term, Term, Term1, Term1):- !.
substitute(_, Term, _, Term):- atomic(Term),!.
substitute(Sub, Term, Sub1, Term1):- 
	Term =.. [F | Args],
	replace_atom(Sub, Args, Sub1, Args1),
	Term1 =.. [F | Args1].

replace_atom(_,[ ],_,[ ]).
replace_atom(Sub,[Term|Terms], Sub1, [Term1|Terms1]) :-
	substitute(Sub, Term, Sub1, Term1),
	replace_atom(Sub, Terms, Sub1, Terms1).


%************************************************************
% Reduccion de la ecuacion a elementos aislados

isolate(X,[=,X,Y],[X,R]) :-
    compute(Y,R),!.            % Cuando X = A

isolate(X,[=,Y,Z],L) :-
    in(X,Z),!,                 % A = f(X) es transformado
    isolate(X,[=,Z,Y],L).      % en f(X) = A

isolate(X,[=,[OP,U,W],Z],L) :-
    in(X,U),!,
    inverse_op(OP,IOP),              % f(X)*A = B es transformado
    isolate(X,[=,U,[IOP,Z,W]],L).    % en f(X) = B/A

isolate(X,[=,[OP,U,W],Z],L) :-
    commutative(OP),!,
    inverse_op(OP,IOP),              % A*f(X)= B es transformado
    isolate(X,[=,W,[IOP,Z,U]],L).    % en f(X) = B/A

isolate(X,[=,[OP,U,W],Z],L) :-
    !,                               % A/f(X) = B es transformado
    isolate(X,[=,W,[OP,U,Z]],L).     % en f(X) = A/B


%************************************************************
% IN es similar a member pero funciona en listas con sublistas

in(X,Y) :-
	flatten(Y,Z),
	member(X,Z).

%************************************************************
% COMPUTE evalua un termino en una ecuacion, regresando un numero
	
compute([OP,P1,P2],R) :-
	atom(OP),
	number(P1),
	number(P2),
	!,
	F=..[OP,P1,P2],
	R is F.

compute([OP,P1,P2],R) :-
	compute(P1,S1),
	compute(P2,S2),
	!,
	F=..[OP,S1,S2],
	R is F.

compute(N,N).
