
-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.

module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros
  )
where

import Util
import Data.List (zipWith4)
import Data.ByteString (length)
data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio casilleros (rangoInferior, rangoSuperior) = Histograma rangoInferior ((rangoSuperior - rangoInferior)/fromIntegral casilleros) (replicate (casilleros+2) 0)

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
--agregar x (Histograma l tamaño ys) = Histograma l tamaño (actualizarElem (floor (x/ (fromIntegral(length ys) * tamaño)+1)) (+1) ys)
agregar valor (Histograma rangoInferior tam ys) = Histograma rangoInferior tam idx (+1) ys
  where idx = min (length ys-1) max ((1 + floor ((valor - rangoInferior) / tam)) 0) 

--calcular idx con where y | y usar actualizarHistograma una sola vez

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma casilleros rango xs = foldr  agregar (vacio casilleros rango) xs

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero min _ _ _) = min

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ max _ _) = max

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.

minimos :: (Float,Float) -> [Int] -> [Float]
minimos (rangoInferior,rangoSuperior) xs = infinitoNegativo:[ (fromIntegral x*(rangoSuperior-rangoInferior))+ rangoInferior | x <- [0..length xs - 2]]

maximos :: (Float,Float) -> [Int] -> [Float]
maximos (rangoInferior,rangoSuperior) xs = rangoInferior :[ (fromIntegral x*(rangoSuperior-rangoInferior))+ rangoSuperior | x <- [0..length xs - 3]] ++ [infinitoPositivo]

porcentajes:: (Float,Float) -> [Int] -> [Float]
porcentajes _ xs = if sum xs == 0
                        then replicate (length xs) 0.0
                        else map (*100) (map (/ fromIntegral (sum xs)) (map fromIntegral xs))

casilleros :: Histograma -> [Casillero]
casilleros (Histograma inicio rango xs) = zipWith4 Casillero (minimos (inicio,inicio+rango) xs) (maximos (inicio,inicio+rango) xs) xs (porcentajes (inicio,inicio+rango) xs)


--minimos (rangoInferior,rangoSuperior) xs = seleccion (rangoInferior,rangoSuperior) xs infinitoNegativo rangoInferior [] 2

--maximos (rangoInferior,rangoSuperior) xs = seleccion (rangoInferior,rangoSuperior) xs rangoInferior rangoSuperior [infinitoPositivo] 3

--seleccion (rangoInferior,rangoSuperior) xs primerElemento ultimoElemento indice = primerElemento:[ (fromIntegral x*(rangoSuperior-rangoInferior))+ rangoInferior | x <- [0..length xs - indice]] ++ ultimoElemento


--hacer lista con los limites intermedios y agregarle -inf y +inf. 