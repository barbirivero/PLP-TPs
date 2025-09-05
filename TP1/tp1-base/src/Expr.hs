module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float -> a)-> --Const
  (Float -> Float -> a) -> --Rango
    (Expr -> Expr ->  a -> a -> a) ->  --Suma
      (Expr -> Expr -> a -> a -> a) -> --Resta
      (Expr -> Expr -> a -> a -> a) -> --Mult
      (Expr -> Expr -> a -> a -> a) -> -- Div
       Expr -> a
recrExpr cCon cRango cSuma cResta cMult cDiv expr = case expr of
                                                    Const c -> cCon c
                                                    Rango l u -> cRango l u
                                                    Suma q1 q2 -> cSuma q1 q2 (rec q1) (rec q2)
                                                    Resta q1 q2 -> cResta q1 q2 (rec q1) (rec q2)
                                                    Mult q1 q2 -> cMult q1 q2 (rec q1) (rec q2)
                                                    Div q1 q2 -> cDiv q1 q2 (rec q1) (rec q2)
                                                    where rec = recrExpr cCon cRango cSuma cResta cMult cDiv

foldExpr :: (Float -> a)-> --Const
  (Float -> Float -> a) -> --Rango
    (a -> a -> a) ->  --Suma
      (a -> a -> a) -> --Resta
      (a -> a -> a) -> --Mult
      (a -> a -> a) -> -- Div
       Expr -> a
foldExpr cCon cRango cSuma cResta cMult cDiv expr = case expr of
                                                    Const c -> cCon c
                                                    Rango l u -> cRango l u
                                                    Suma q1 q2 -> cSuma (rec q1) (rec q2)
                                                    Resta q1 q2 -> cResta (rec q1) (rec q2)
                                                    Mult q1 q2 -> cMult (rec q1) (rec q2)
                                                    Div q1 q2 -> cDiv (rec q1) (rec q2)
                                                    where rec = foldExpr cCon cRango cSuma cResta cMult cDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> Gen -> (Float,Gen)
eval expr g = (foldExpr cCon cRango cSuma cResta cMult cDiv expr) g
  where
    cCon :: Float -> (Gen -> (Float,Gen))
    cCon c gen = (c, gen)

    cRango :: Float -> Float -> (Gen -> (Float,Gen))
    cRango l u gen = dameUno (l,u) gen

    cSuma, cResta, cMult, cDiv :: (Gen -> (Float,Gen)) -> (Gen -> (Float,Gen)) -> (Gen -> (Float,Gen))

    cSuma f1 f2 gen =
      case f1 gen of
        (v1,g1) -> case f2 g1 of
          (v2,g2) -> (v1 + v2, g2)

    cResta f1 f2 gen =
      case f1 gen of
        (v1,g1) -> case f2 g1 of
          (v2,g2) -> (v1 - v2, g2)

    cMult f1 f2 gen =
      case f1 gen of
        (v1,g1) -> case f2 g1 of
          (v2,g2) -> (v1 * v2, g2)

    cDiv f1 f2 gen =
      case f1 gen of
        (v1,g1) -> case f2 g1 of
          (v2,g2) -> (v1 / v2, g2)



--eval expr g = (foldExpr id (\a b-> fst (dameUno (a,b) g)) (+) (-) (*) (/) expr, snd (dameUno (1,2) g))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
--armarHistograma :: Int -> Int -> Gen -> (Float,Gen) -> Gen -> (Histograma,Gen)
armarHistograma m n f g = (histograma m (rango95 (fst (muestra f n g))) (fst (muestra f n g)), g)

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
