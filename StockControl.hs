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
updateStock :: Stock -> String -> Int -> Stock
updateStock (ROOTNODE l) p n = ROOTNODE (updateList l p n)
updateStock (INNERNODE c l) p n
  | c == head p = INNERNODE c (updateList l (tail p) n)
  | otherwise = INNERNODE c l
updateStock (INFONODE _) [] n = INFONODE n
updateStock s _ _ = s

updateList :: [Stock] -> String -> Int -> [Stock]
updateList [] p n = [INNERNODE (head p) [INFONODE n]]
updateList (x:xs) p n
  | head p == getCharFromStock x = (updateStock x p n):xs
  | otherwise = x:(updateList xs p n)

getCharFromStock :: Stock -> Char
getCharFromStock (INNERNODE c _) = c
getCharFromStock _ = '\0'


-----------------------
-- FUNCIÓN LISTSTOCK --
-----------------------
listStock :: Stock -> String -> [(String,Int)]
listStock s p = map extractInt $ bt isINFONODE (children p) (("", s))

-- Función auxiliar que determina si un Stock es INFONODE
isINFONODE :: (String, Stock) -> Bool
isINFONODE (_, INFONODE _) = True
isINFONODE _ = False


children :: String -> (String, Stock) -> [(String, Stock)]
children _ ("", ROOTNODE l) = [("", node) | node <- l]
children p (s, INNERNODE c l)
  | p == take (length p) (s ++ [c]) = [ (s ++ [c], node) | node <- l ] ++ children p (s, INNERNODE c l)
  | otherwise = [("", node) | node <- l]
children _ (s, INFONODE n) = [(s, INFONODE n)]
children _ _ = []


extractInt :: (String, Stock) -> (String, Int)
extractInt (s, INFONODE n) = (s, n)
extractInt _ = error "No INFONODE found"

-- FUNCIÓN GENÉRICA DE BACKTRACKING --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt eS c n
  | any eS (c n) = c n
  | otherwise = concatMap (bt eS c) (c n)




