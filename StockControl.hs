module StockControl where


data Stock = ROOTNODE [Stock] | INNERNODE Char [Stock] | INFONODE Int
  deriving (Show,Read,Eq)

-------------------------
-- FUNCIÓN CREATESTOCK --
-------------------------

-- FUNCIÓN QUE DEVUELVE UN STOCK VACÍO --
createStock :: Stock
createStock = ROOTNODE[]

---------------------------
-- FUNCIÓN RETRIEVESTOCK --
---------------------------

-- FUNCIÓN QUE DEVUELVE EL NÚMERO DE UNIDADES DE UN PRODUCTO EN EL STOCK --
-- SI NO ESTÁ, DEBERÁ DEVOLVER -1                                        --

retrieveStock :: Stock -> String -> Int
retrieveStock (ROOTNODE []) _ = -1
retrieveStock (ROOTNODE _) [] = -1
retrieveStock (ROOTNODE l) p = stockToInt (profCheck l p)
retrieveStock (INNERNODE _ l) p = stockToInt (profCheck l p)
retrieveStock (INFONODE n) [] = n
retrieveStock (INFONODE _) (_:_) = -1

-- Recorrido en profundidad comprobando si los caracteres son iguales
profCheck :: [Stock] -> String -> Stock
profCheck [] _ = (ROOTNODE[])
profCheck ((ROOTNODE _):_) [] = (ROOTNODE[])
profCheck ((ROOTNODE l):_) p = profCheck l p
profCheck ((INFONODE n):_) [] = (INFONODE n)
profCheck ((INFONODE _):xs) (p:ps) = profCheck xs (p:ps)
profCheck ((INNERNODE _ _):_) [] = (INFONODE 0)
profCheck ((INNERNODE c l):xs) p 
  | c == (head p) = profCheck l (tail p)
  | otherwise = profCheck xs p

-- Devuelve el numero del nodo INFONODE o -1 en otro caso
stockToInt :: Stock -> Int
stockToInt (INFONODE n)
  | n > 0 = n
  | otherwise = -1
stockToInt (ROOTNODE _) = -1
stockToInt (INNERNODE _ _) = -1
-------------------------
-- FUNCIÓN UPDATESTOCK --
-------------------------

-- FUNCIÓN QUE MODIFICA EL VALOR ASOCIADO A UN PRODUCTO EN EL STOCK --
-- SÓLO PUEDE ALMACENAR NÚMEROS MAYORES O IGUALES A 0               --

updateStock :: Stock -> String -> Int -> Stock


-----------------------
-- FUNCIÓN LISTSTOCK --
-----------------------

-- FUNCIÓN QUE DEVUELVE UNA LISTA PARES PRODUCTO-EXISTENCIA --
-- DEL CATÁLOGO QUE COMIENZAN POR LA CADENA PREFIJO p       --
listStock :: Stock -> String -> [(String,Int)]


-- FUNCIÓN GENÉRICA DE BACKTRACKING --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    eS             c             n
  | eS n      = [n]
  | otherwise = concat (map (bt eS c) (c n))



