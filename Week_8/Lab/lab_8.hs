import Data.List
import System.Random (StdGen, newStdGen, randomRs)

-- TASK 1
implies :: Bool -> Bool -> Bool
implies False _ = True
implies True val = val

-- TASK 2
imply :: [Bool] -> Bool -> Bool
imply lst val = foldr implies val lst

divides :: Int -> Int -> Bool
divides b a = a `mod` b == 0 

-- TASK 3
cond :: [(Bool, a)] -> a -> a
cond [] ans = ans
cond ((True, val):_) _ = val
cond (x:xs) ans = cond xs ans

-- TASK 4
randomDigits :: StdGen -> [Int]
randomDigits g = randomRs (0, 9) g

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n lst = take n lst : chunksOf n (drop n lst)

-- TASK 5
randomMatrix :: Int -> StdGen -> [[Int]]
randomMatrix 0 _ = []
randomMatrix n g = chunksOf n (take (n * n) (randomDigits g))

-- TASK 6
every2nd :: [a] -> [a]
every2nd [] = []
every2nd (_:xs) = take 1 xs ++ every2nd (drop 1 xs)

main :: IO()
main = do
    -- TASK 1
    -- print (implies (10 `mod` 6 == 0) (10 `mod` 0 == 0))

    -- TASK 2
    -- print (imply (map (`divides` 100) [20, 4]) (3 `divides` 100))
    -- print (imply (map (`divides` 100) [3, undefined]) (4 `divides` 100))
    -- print (imply [3 `divides` 100] (4 `divides` 100))
    -- print (imply [] (4 `divides` 100))

    -- TASK 3
    -- print (let n = 12 in cond
    --         [ (n == 0, "no apples")
    --         , (n == 1, "one apple")
    --         , (n < 0, error "invalid input")]
    --         "many apples")

    -- print (cond [] "hello")
    
    -- TASK 4
    -- print (chunksOf 3 [1..10])
    -- print (chunksOf 3 "Hello, world!")

    -- TASK 5
    -- g <- newStdGen
    -- mapM_ print (randomMatrix 3 g)

    -- TASK 6
    print (every2nd [1..10])
    print (every2nd "Hello, world!")
    print (every2nd [undefined, 2])
    print (take 3 (every2nd [1..]))