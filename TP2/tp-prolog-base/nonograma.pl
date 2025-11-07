% Ejercicio 1
%matriz(+Filas, +Columnas ?Matriz)
matriz(0, _, []).
matriz(Filas, Columnas, [Head|Tail]):-
	Filas > 0,
	F1 is Filas-1,
	length(Head, Columnas),
	matriz(F1, Columnas, Tail).

% Ejercicio 2
%replicar(+Elem, +N ?Lista)
replicar(_, 0, []).
replicar(Elemento, N, [Elemento|Tail]) :-
	N > 0,
	N1 is N - 1,
	replicar(Elemento, N1, Tail).

% Ejercicio 3
%transponer(+M, -MT)
transponer([], []).
transponer([[]|_], []).
transponer(Matriz, [Columna|RestoTranspuesta]):-
	sacarPrimerColumna(Matriz, Columna, RestoMatriz),
    transponer(RestoMatriz, RestoTranspuesta).

%sacarPrimerColumna(?M, ?C, ?R)
sacarPrimerColumna(Matriz, Columna, Resto) :-
    maplist(listaComp, Matriz, Columna, Resto).

%listaComp(?L, ?X, ?XS)
listaComp([X|XS], X, XS).

% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).

% Ejercicio 4
%pintadasValidas(+R)
pintadasValidas(r(RestricPintadas, Celdas)):-
	crearResticcionesDeBlancas(RestricPintadas, Celdas, RestriccBlancas),
	restriccionesValidas(RestriccBlancas),
	pintarFila(RestriccBlancas, RestricPintadas, Celdas).

%crearResticcionesDeBlancas(+RP, +C, -RB):-
crearResticcionesDeBlancas(RPintadas, Celdas, RBlancas):-
	length(RPintadas,CantRestriccionesPintadas),
	CantRestriccionesBlancas is CantRestriccionesPintadas + 1,
	length(Celdas,Size),
	sum_list(RPintadas,CantPintadas),
	CantBlancas is Size - CantPintadas,
	generarListaQueSuma(CantRestriccionesBlancas, CantBlancas, RBlancas).

%generarListaQueSuma(+N,+S,?L). 
generarListaQueSuma(0, 0, []). 
generarListaQueSuma(N, Suma, [X|XS]) :-
    N > 0,
    N1 is N - 1,
    between(0,Suma, X),
    Resto is Suma - X,
    generarListaQueSuma(N1, Resto, XS).

%restriccionesValidas(+L)
restriccionesValidas([]).
restriccionesValidas([X|XS]):- X>=0, restoValido(XS).

%restoValido(+L)
restoValido([]).
restoValido([X]):- X>=0.
restoValido([X1, X2|XS]):- X1 >= 1, restoValido([X2|XS]).

%pintarFila(+RB,+RP,-C)
pintarFila([Ultima],[], Celdas):- 
	replicar(o, Ultima, Celdas).
pintarFila([CantBlancas|ResBlanca], [CantPintadas|ResPintada], Celdas):-
	replicar(o, CantBlancas, ListaBlanca),
	replicar(x, CantPintadas, ListaPintada),
	append(ListaBlanca, ListaPintada, Segmento),
	append(Segmento, Resto, Celdas),
	pintarFila(ResBlanca, ResPintada, Resto).

% Ejercicio 5
resolverNaive(nono(_,RS)) :-
	maplist(pintadasValidas, RS).

% Ejercicio 6
pintarObligatorias(r(Restricciones, Celdas)) :-
    findall(Celdas, pintadasValidas(r(Restricciones, Celdas)), TodasLasPintadas),
    combinarTodas(TodasLasPintadas, Celdas).

% combinarTodas(+Pintadas, ?Celdas)
combinarTodas([Primera|Resto], Celdas) :-
    combinarTodasLasPintadas(Resto, Primera, Celdas).

% combinarTodasLasPintadas(+Pintadas, +Acum, -Resultado)
combinarTodasLasPintadas([], Acum, Acum).
combinarTodasLasPintadas([Pintada|Resto], Acum, Resultado) :-
    combinarDosPintadas(Acum, Pintada, NuevoAcum),
    combinarTodasLasPintadas(Resto, NuevoAcum, Resultado).

% combinarDosPintadas(+P1, +P2, -Resultado)
combinarDosPintadas([], [], []).
combinarDosPintadas([C1|T1], [C2|T2], [R|TR]) :-
    combinarCelda(C1, C2, R),
    combinarDosPintadas(T1, T2, TR).

% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7
deducir1Pasada(nono(_,[])).
deducir1Pasada(nono(M,[H|T])) :- pintarObligatorias(H), deducir1Pasada(nono(M,T)).

% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).

% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).

% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo mas cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).

% Ejercicio 8

restriccionConMenosLibres(nono(_, Rs), R) :- 
    member(R, Rs),
    cantidadVariablesLibres(R, N),
    N > 0,
    not((member(R2, Rs), 
         cantidadVariablesLibres(R2, N2), 
         N2 > 0, 
         N2 < N)).

% Ejercicio 9
resolverDeduciendo(NN) :-
    deducirVariasPasadas(NN),
    cantidadVariablesLibres(NN, 0), !.

resolverDeduciendo(NN) :-
    restriccionConMenosLibres(NN, R), !,
    pintadasValidas(R),
    resolverDeduciendo(NN).

% Ejercicio 10
solucionUnica(nono(M, RS)) :-
    findall(M, resolverDeduciendo(nono(M, RS)), [_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0, NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1, NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2, NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3, NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4, NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5, NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6, NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7, NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8, NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9, NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').


%------%
/*
Ejercicio 11: Es reversible %replicar(+Elem, +N ?Lista) en el segundo Parametro?
No, ya que hay clausulas con operacion aritmeticas que necesitan que los argumentos esten instanciadas, en particular ">" y "is", y ambas usan N.


Ejercicio 12

0  | 2x3   | Si | Si
1  | 5x5   | Si | Si
2  | 5x5   | Si | Si
3  | 10x10 | Si | Si
4  | 5x5   | Si | Si
5  | 5x5   | Si | No
6  | 5x5   | Si | Si
7  | 10x10 | Si | Si
8  | 10x10 | Si | Si
9  | 5x5   | Si | Si
10 | 5x5   | No | No
11 | 10x10 | Si | Si
12 | 15x15 | Si | Si
13 | 11x5  | -- | --
14 | 4x4   | Si | No


*/