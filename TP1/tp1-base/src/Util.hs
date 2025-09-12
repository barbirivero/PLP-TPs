module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha longitud string = if length string < longitud
                    then replicate (longitud - length string) ' ' ++ string
                    else string


-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.

actualizarElemOrdenCambiado :: (a -> a) -> [a] -> (Int -> [a])
actualizarElemOrdenCambiado f = foldr (\x rec indice -> if indice == 0 then f x : rec (indice-1) else x : rec (indice-1))
                                        (const [])

actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem indice f lista = actualizarElemOrdenCambiado f lista indice



-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
