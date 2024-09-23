% Reglas del sistema experto - Sofia Ruvalcaba de la Noval

% Regla: derivada de una constante es 0.
d(C, _, 0) :- number(C).

% Regla: derivada de una variable con respecto a sí misma es 1.
d(X, X, 1).

% Regla: derivada de una variable con respecto a otra variable es 0.
d(Y, X, 0) :- atom(Y), Y \= X.

% Regla: derivada de una suma es la suma de las derivadas.
d(U + V, X, DU + DV) :- 
    d(U, X, DU),
    d(V, X, DV).

% Regla: derivada de una resta es la resta de las derivadas.
d(U - V, X, DU - DV) :- 
    d(U, X, DU),
    d(V, X, DV).

% Regla: derivada de un producto por una constante.
d(C * U, X, C * DU) :- 
    number(C),
    d(U, X, DU).

% Regla: derivada de una potencia X^N es N*X^(N-1).
d(X^N, X, N * X^M) :- 
    number(N),
    M is N - 1.

% Regla general de la potencia (derivada de (f(x))^n)
d((U^N), X, N * U^(N1) * DU) :-
    number(N),
    N1 is N - 1,
    d(U, X, DU).

% Simplificación para multiplicaciones por 1 y por 0
simplificar(0 * _, 0).
simplificar(1 * X, X).
simplificar(C * 1, C).
simplificar(C * 0, 0).
simplificar(0 + X, X).
simplificar(X + 0, X).
simplificar(0 - X, -X).
simplificar(X - 0, X).
simplificar(X, X).

% Reglas para funciones trascendentales

% Derivada de sen(x)
d(sen(U), X, cos(U) * DU) :-
    d(U, X, DU).

% Derivada de cos(x)
d(cos(U), X, -sen(U) * DU) :-
    d(U, X, DU).

% Derivada de exp(x)
d(exp(U), X, exp(U) * DU) :-
    d(U, X, DU).

% Derivada de log(x)
d(log(U), X, (1 / U) * DU) :-
    d(U, X, DU).

% Reglas de la cadena

% Regla de la cadena para funciones compuestas
d(U(V), X, DU * DV) :-
    d(U, V, DU),
    d(V, X, DV).

% Ejemplos de consultas de derivadas
:- begin_tests(derivadas).

test(derivada_de_constante) :-
    d(5, x, D),
    assertion(D = 0).

test(derivada_simple) :-
    d(x^2, x, D),
    simplificar(D, R),
    assertion(R = 2 * x).

test(derivada_multiplicacion_constante) :-
    d(3 * x^2, x, D),
    simplificar(D, R),
    assertion(R = 6 * x).

test(derivada_suma) :-
    d(3 * x^2 + 5 * x + 7, x, D),
    simplificar(D, R),
    assertion(R = 6 * x + 5).

test(derivada_trigonometrica) :-
    d(sen(x), x, D),
    simplificar(D, R),
    assertion(R = cos(x)).

test(derivada_cadena) :-
    d(sen(3 * x + 2), x, D),
    simplificar(D, R),
    assertion(R = cos(3 * x + 2) * 3).

test(derivada_exponencial) :-
    d(exp(x), x, D),
    simplificar(D, R),
    assertion(R = exp(x)).

:- end_tests(derivadas).