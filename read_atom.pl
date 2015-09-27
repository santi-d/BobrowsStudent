% File: read_atom.pl
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Appendix B


% read_atomics(-Atomics)
%  Lee una linea de texto introducida por pantalla y lo tokeniza
%  en una lista de atomos: [this,is,an,example].

read_atomics(Atomics) :-
   read_char(FirstC,FirstT),
   complete_line(FirstC,FirstT,Atomics).


% read_char(-Char,-Type)
%  Lee un caracter y devuelve el tipo de caracter

read_char(Char,Type) :-
   get0(C), 				% extrae el valor ascii del primer caracter escrito por teclado y lo almacena en la variable C
   char_type(C,Type,Char). 

% complete_line(+FirstC,+FirstT,-Atomics)
%  Realiza el tokenizado del resto de la linea dado un FirstC (primer caracter) y FirstT (su tipo)

complete_line(_,end,[]) :- !.					% Detiene la lectura al encontrarse al final de linea

complete_line(_,blank,Atomics) :-				% Si se trata de un blank hace un quiebre para comenzar otra palabra en la lista
   !,
   read_atomics(Atomics).

complete_line(FirstC,special,[A|Atomics]) :-	% Cualquier caracter considerado especial se convertira en un elemento aislado de la lista
   !,
   name(A,[FirstC]),
   read_atomics(Atomics).

complete_line(FirstC,alpha,[A|Atomics]) :-
   complete_word(FirstC,alpha,Word,NextC,NextT),
   name(A,Word),
   complete_line(NextC,NextT,Atomics).


% complete_word(+FirstC,+FirstT,-List,-FollC,-FollT)
%  Dado FirstC y FirstT seguira leyendo la linea ingresada por teclado y colocando las palabras en la lista.

complete_word(FirstC,alpha,[FirstC|List],FollC,FollT) :-
   !,
   read_char(NextC,NextT),
   complete_word(NextC,NextT,List,FollC,FollT).


complete_word(FirstC,FirstT,[],FirstC,FirstT).
   % donde FirstT no es alpha


% char_type(+Code,?Type,-NewCode)
%  Dependiendo del codigo ASCII obtenido, clasifica el caracter como:
%  'end' (of line/file)
%  'blank'
%  'alpha'(numeric)
%  'special'

char_type(10,end,10) :- !.					% UNIX EOF
char_type(13,end,13) :- !.					% DOS EOF
char_type(-1,end,-1) :- !.					% get0 EOF

char_type(Code,blank,32) :-					% blanks y otros caracteres de control
  Code =< 32,
  !.
 
char_type(Code,alpha,Code) :-				% digitos
  48 =< Code, Code =< 57,
  !.

char_type(Code,alpha,Code) :-				% subguin
  95 is Code,
  !.

char_type(Code,alpha,Code) :-				% letras minusculas
  97 =< Code, Code =< 122,
  !.

%char_type(Code,alpha,Code) :- 				% comilla simple
%  39 is Code,								% para cuando se haga el posesivo no se pierda el caracter
%  !.

char_type(Code,alpha,NewCode) :-			% letras mayusculas *
  65 =< Code, Code =< 90,
  !,
  NewCode is Code + 32.            			% * se vuelven minusculas

char_type(Code,special,Code).      			% cualquier otro caracter se considera special.
