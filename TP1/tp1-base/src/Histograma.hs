{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
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
    casilleros,
  )
where

import Util
import Data.List (zipWith4)
data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l ((u-l)/fromIntegral n) (replicate (n+2) 0)

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
--agregar x (Histograma l tamaño ys) = Histograma l tamaño (actualizarElem (floor (x/ (fromIntegral(length ys) * tamaño)+1)) (+1) ys)
agregar x (Histograma l tam ys)
  | x < l     = Histograma l tam (actualizarElem 0 (+1) ys)
  | x >= l + tam * fromIntegral (length ys - 2) = Histograma l tam (actualizarElem (length ys - 1) (+1) ys)
  | otherwise = 
      let idx = 1 + floor ((x - l) / tam)
      in Histograma l tam (actualizarElem idx (+1) ys)


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r xs = foldr  agregar (vacio n r) xs

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
minimos (l,u) xs = infinitoNegativo:[ (fromIntegral x*(u-l))+ l | x <- [0..length xs - 2]]

maximos :: (Float,Float) -> [Int] -> [Float]
maximos (l,u) xs = l :[ (fromIntegral x*(u-l))+ u | x <- [0..length xs - 3]] ++ [infinitoPositivo]

porcentajes:: (Float,Float) -> [Int] -> [Float]
porcentajes (l,u) xs = if sum xs == 0
                        then replicate (length xs) 0.0
                        else map (*100) (map (/ fromIntegral (sum xs)) (map fromIntegral xs))

casilleros :: Histograma -> [Casillero]
casilleros (Histograma n r xs) = zipWith4 Casillero (minimos (n,n+r) xs) (maximos (n,n+r) xs) xs (porcentajes (n,n+r) xs)
