module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = if length s < n 
                    then replicate (n - length s) ' ' ++ s 
                    else s  


-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.

actualizarElemOrdenCambiado :: (a -> a) -> [a] -> (Int -> [a])
actualizarElemOrdenCambiado f = foldr (\x rec n -> if n == 0 then [f x] ++ rec (n-1) else x : rec (n-1))
                                        (const [])

actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = actualizarElemOrdenCambiado f xs n



-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
