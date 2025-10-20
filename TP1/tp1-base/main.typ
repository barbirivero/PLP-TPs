#set text(lang: "ES", region:"AR")
#align(center, text(17pt)[*Trabajo Práctico - PLP*])
== Ejercicio 12.

Queremos probar que $forall e:: "Expr. cantLit "e  = "S(cantOp "e")"$ 

Es decir _*QVQ*_

P($e$):  $"cantLit "e = "S(cantOp "e")"$ para todo e.

Para probarlo hacemos inducción estructural sobre la expresión e.

#underline[Caso base 1:] $"P(const a")$

#align(center)[ 
$"cantLit const a" = "S (cantOp const a)"$

${"L1"} "S (Zero)" = "S (Zero)" {"O1"}$

Tenemos la misma expresión en ambos lados del "=" por lo que queda probado el caso base 1.
]

#underline[Caso base 2:] $"P(Rango a b")$

#align(center)[ 
$"cantLit Rango a b" = "S (cantOp Rango a b)"$

${"L2"} "S (Zero)" = "S (Zero)" {"O2"}$

Tenemos la misma expresión en ambos lados del "=" por lo que queda probado el caso base 2.
]

Para hacer el paso inductivo tomo como Hipótesis inductiva a: 
- $"P"(a$): $"cantLit "a = "S(cantOp "a")"$
- $"P"(b$): $"cantLit "b = "S(cantOp "b")"$

Quiero probar que $forall a:: "Expr."$$forall b:: "Expr."$ $"P"(a) and "P"(b) => "P"("Suma" a" "b )   $ 

#underline[Paso inductivo:] $"P(Suma a b")$

#align(center)[ 
$"cantLit Suma a b" = "S (cantOp Suma a b)"$

${"L3"} "suma (cantLit a) (cantLit b)" = " S ( S (suma (cantOp a) (cantOp b)))" {"O3"}$

Utilizo en el lado izquierdo las hipótesis inductivas.

$"suma (S(cantOp a)) (S(cantOp b))" = " S ( S (suma (cantOp a) (cantOp b)))"$

${"S2"} "S(suma (cantOp a) (S(cantOp b)))" = " S ( S (suma (cantOp a) (cantOp b)))"$

${"Conmut"} "S (suma (S (cantOp b)) (cantOp a))"=" S ( S (suma (cantOp a) (cantOp b)))"$

${"S2"} "S ( S (suma (cantOp b) (cantOp a)))"=" S ( S (suma (cantOp a) (cantOp b)))"$

${"Conmut"} "S ( S (suma (cantOp a) (cantOp b)))"=" S ( S (suma (cantOp a) (cantOp b)))"$

Tenemos la misma expresión en ambos lados del "=" por lo que queda probado el paso inductivo.
]

Demostrado los casos base y el caso inductivo sobre la suma queda demostrada la propiedad (los casos para el resto de operaciones son análogos al de suma).
