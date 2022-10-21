import Prelude

{-# HLINT ignore "Use first" #-}

-- TASK 1
-- A
isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _ = False

-- B
insert :: Int -> [Int] -> [Int]
insert v [] = [v]
insert v (x:xs)
    | v <= x = v:x:xs
    | otherwise = x : insert v xs

-- C
separateBy :: a -> [a] -> [a]
separateBy _ [] = []
separateBy s [x] = [x]
separateBy s (x:xs) = x:s:separateBy s xs

-- D
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot _ [] = ([], [])
splitWhenNot f (x:xs)
    | f x = (x:fst (splitWhenNot f xs), snd (splitWhenNot f xs))
    | otherwise = ([], x:xs)

-- E
splitWhenTrue :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenTrue _ [] = ([], [])
splitWhenTrue f (x:xs)
    | f x = ([], xs)
    | otherwise = (x:fst (splitWhenTrue f xs), snd (splitWhenTrue f xs))

groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy f lst = case splitWhenTrue f lst of
    ([], []) -> [[]]
    (x, []) -> [x]
    (x, xs) -> x : groupsSeparatedBy f xs

-- F
replicateWithPos :: [a] -> [a]
replicateWithPos = replicateN 1
    where
        replicateN _ [] = []
        replicateN n (x:xs) = replicate n x ++ replicateN (n+1) xs

-- TASK 2
-- A
lucas :: [Int]
lucas = 2 : 1 : zipWith (+) lucas (tail lucas)

-- B
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 x = x : approximationsOfRoot2 (x - (x * x - 2) / (2 * x))


main :: IO()
main = do
    putStrLn "----TASK 1----"
    putStrLn "\nA"
    print (isSingleton [1])
    print (isSingleton [1..])
    print (isSingleton [[1..]])

    putStrLn "\nB"
    print (insert 3 [1,2,5,7])
    print (insert 3 [0,1,1])
    print (take 5 (insert 3 [1..]))

    putStrLn "\nC"
    print (separateBy ',' "hello")
    print (take 5 (separateBy 0 [1..]))

    putStrLn "\nD"
    print (splitWhenNot (/= ' ') "Hello, world!")
    print (take 10 (fst (splitWhenNot (< 100) [1..])))
    print (take 10 (snd (splitWhenNot (< 100) [1..])))
    print (take 10 (fst (splitWhenNot (> 0) [1..])))

    putStrLn "\nE"
    print (groupsSeparatedBy (== ' ') "Here are some words!")
    print (take 3 (groupsSeparatedBy (\n -> n `mod` 4 == 0) [1..]))

    putStrLn "\nF"
    print (replicateWithPos [1..3])
    print (replicateWithPos "Hello")
    print (take 10 (replicateWithPos [1..]))

    putStrLn "\n\n----TASK 2----"
    putStrLn "\nA"
    print (take 10 lucas)

    putStrLn "\nB"
    print (take 5 (approximationsOfRoot2 1))