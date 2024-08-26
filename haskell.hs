 --------------------- GUIA 0 -------------------

valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

{- o sino otra forma sería: -}
valorAbsoluto2 :: Float -> Float
valorAbsoluto2 x = if x < 0 then -x else x

bisiesto :: Int -> Bool
bisiesto x | x `mod` 100 == 0 && x `mod` 400 == 0 = True
           | x `mod` 4 == 0 = True
           | otherwise = False


factorial :: Int -> Int            -- solo funciona para numeros positivos y cero
factorial 0 = 1
factorial x = x * factorial (x-1)


desde :: Int-> [Int]
desde n = n : desde (n + 1)


cantDivisores :: Int -> Int
cantDivisores x = length [n | n <- [2..x-1], x `mod` n == 0]

esPrimo :: Int -> Bool
esPrimo x | cantDivisores x == 0 = True
          | otherwise = False

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = length [n | n <- [2..x-1], esPrimo n, x `mod` n == 0]


inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing
          | otherwise = Just (1/x)


aEntero :: Either Int Bool -> Int
aEntero (Left i) = i
aEntero (Right b) = if b then 1 else 0


mapp :: (a -> b) -> [a] -> [b]
mapp f [] = []
mapp f (x:xs) = (f x) : (mapp f xs)

difPromedio :: [Float] -> [Float]
difPromedio xs = mapp (\x -> x - promedio xs) xs
    where
        promedio s = sum s / fromIntegral (length s)


todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs) == True


evaluarEnCero :: (Integer -> t) -> t
evaluarEnCero = \f -> f 0

max2 :: Ord a => (a, a) -> a
max2 (x, y) | x >= y = x
            | otherwise = y

--------------------------------------- GUIA 1 --------------------------------------

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y


sumFoldr :: Num a => [a] -> a
sumFoldr s = foldr (+) 0 s



elemFoldr :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldr x = foldr (\y ac -> (x == y) || ac) False



concatFoldr :: Foldable t => t a -> [a] -> [a]
concatFoldr x y = foldr (:) y x


filterFoldr :: Foldable t => (a -> Bool) -> t a -> [a]
filterFoldr f = foldr (\x ac -> if f x then x : ac else ac) []


mapFoldr :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapFoldr f = foldr (\x ac -> f x : ac) []


mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x ac -> if f x ac then x else ac)



sumasParciales :: Num a => [a] -> [a]
sumasParciales = sumasParcialesAux 0
    where sumasParcialesAux _ [] = []
          sumasParcialesAux ac (x:xs) = (ac + x) : sumasParcialesAux (ac + x) xs


sumaAlt :: Num a => [a] -> a
sumaAlt = fst . foldr (\x (ac, i) -> (if even i then ac + x else ac - x, i + 1)) (0,0)


largo :: [a] -> Int
largo [] = 0
largo (_:xs) = 1 + largo xs

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = head . foldr (\x ac -> if even (largo ac) then ac ++ [-x] else ac ++ [x]) [] -- ESTE NO FUNCIONA BIEN


permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap (\(x, xs') -> map (x :) (permutaciones xs')) (seleccionar xs)


seleccionar :: [a] -> [(a, [a])]                                                        -- toda copiada xdd
seleccionar [] = []
seleccionar (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (seleccionar xs)