-- | A value with explicitly separated computation steps.
data Iter a
    = Done a -- ^ Final (computed) value.
    | Step (Iter a) -- ^ A computation that requires at least one more step.
        deriving (Show) -- for printing

factorialIter :: Int -> Iter Int
factorialIter = go 1
    where
        go current n
            | n <= 1 = Done current
            | otherwise = Step (go (n * current) (n - 1))
        
-- TASK 1

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : insert x ys

-- TASK 2

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _ = False

approximate :: (a -> Bool) -> (a -> a) -> a -> Iter a
approximate p f x
    | p x = Done x
    | otherwise = Step (approximate p f (f x))


-- TASK 3
-- SUBTASK A

eval :: Iter a -> a
eval (Done x) = x
eval (Step x) = eval x

-- SUBTASK B

limit :: Int -> Iter a -> Iter (Maybe a)
limit 0 _ = Done Nothing
limit n (Done x) = Done (Just x)
limit n (Step x) = Step (limit (n - 1) x)

-- SUBTASK C

partialEval :: Int -> Iter a -> Iter a
partialEval 0 x = x
partialEval n (Done x) = Done x
partialEval n (Step x) =  partialEval (n - 1) x

-- SUBTASK D

steps :: Iter a -> Int
steps (Done x) = 0
steps (Step x) = 1 + steps x


-- TASK 4
-- SUBTASK A

mapIter :: (a -> b) -> Iter a -> Iter b
mapIter f (Done x) = Done (f x)
mapIter f (Step x) = Step (mapIter f x)

-- SUBTASK B

joinIter :: Iter (Iter a) -> Iter a
joinIter (Done x) = x
joinIter (Step x) = Step (joinIter x)

-- TASK 5

insertIter :: Int -> [Int] -> Iter [Int]
insertIter x [] = Done [x]
insertIter x (y:ys)
    | x <= y = Step (Done (x : y : ys))
    | otherwise = Step (mapIter (y:) (insertIter x ys))


-- TASK 6

insertionSortIter :: [Int] -> Iter [Int]
insertionSortIter lst = go lst (Done [])
    where
        go [] sorted = sorted
        go lst (Step x) = Step (go lst x)
        go (x:xs) sorted =  go xs (insertIter x (eval sorted))


main :: IO()
main = do
    putStrLn "TASK 1"
    print (insert 3 [1,2,5,7])
    print (insert 3 [0,1,1])
    print (take 5 (insert 3 [1..]))

    putStrLn "\n\nTASK 2"
    print (approximate (\x -> x^2 < 1) (/ 2) 3)
    print (approximate (\x -> x^2 < 0.01) (/ 2) 3)
    print (approximate isSingleton (drop 1) [1..3])

    putStrLn "\n\nTASK 3"
    putStrLn "SUBTASK A"
    print (eval (approximate (\x -> x^2 < 0.01) (/ 2) 10))

    putStrLn "\nSUBTASK B"
    print (limit 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3))
    print (limit 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3))
    print (limit 0 (approximate (\x -> x^2 < 0.01) (/ 2) 3))

    putStrLn "\nSUBTASK C"
    print (partialEval 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3))
    print (partialEval 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3))

    putStrLn "\nSUBTASK D"
    print (steps (approximate (\x -> x^2 < 0.01) (/ 2) 3))

    putStrLn "\n\nTASK 4"
    putStrLn "SUBTASK A"
    print (mapIter (+1) (Done 3))
    print (mapIter (+1) (Step (Step (Done 3))))

    putStrLn "\nSUBTASK B"
    print (joinIter (Step (Done (Step (Done 3)))))

    putStrLn "\n\nTASK 5"
    print (insertIter 1 [2, 3])
    print (insertIter 4 [2, 3])

    putStrLn "\n\nTASK 6"
    print (insertionSortIter [1..4])
    print (insertionSortIter [4, 3..1])
    print (steps (insertionSortIter [1..10]))
    print (steps (insertionSortIter [10,9..1]))