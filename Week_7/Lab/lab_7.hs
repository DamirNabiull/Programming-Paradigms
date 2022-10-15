import Text.Read (readMaybe)

-- TASK 1
parseIntValue :: String -> Maybe Int
parseIntValue value =
    case (readMaybe value) :: Maybe Int of
        Nothing -> Nothing
        Just value_int -> Just value_int

getIntFromLine :: IO Int
getIntFromLine = do
    putStrLn "Print number:"
    value <- getLine
    case (parseIntValue value) of
        Nothing -> do
            putStrLn "Print the correct number"
            getIntFromLine
        Just val_a -> do
            putStrLn "\n"
            return val_a

task1 :: IO ()
task1 = do
    putStrLn "Print number 1:"
    val_a <- getIntFromLine
    putStrLn "Print number 2:"
    val_b <- getIntFromLine
    print (val_a + val_b)


-- TASK 2 
itterativeNumberInput :: Int -> Int -> IO Int
itterativeNumberInput n ans
    | n == 0 = return ans
    | otherwise = do
        putStrLn "Print new number:"
        val <- getIntFromLine
        itterativeNumberInput (n - 1) (ans + val)


task2 :: IO ()
task2 = do
    putStrLn "Print number n:"
    val_n <- getIntFromLine
    ans <- itterativeNumberInput val_n 0
    print(ans)


-- TASK 3
replicateIO_ :: Int -> IO () -> IO ()
replicateIO_ n act
    | n == 0 = return ()
    | otherwise = do
        act
        replicateIO_ (n - 1) act


-- TASK 4
replicateIO :: Int -> IO a -> IO [a]
replicateIO n act
    | n == 0 = return []
    | otherwise = do
        x <- act
        xs <- replicateIO (n - 1) act
        return (x:xs)

task4 :: IO ()
task4 = do
    a <- replicateIO 5 (return 5)
    print(a)


-- TASK 5
isTwo :: Int -> IO String
isTwo 2 = return "Yes"
isTwo _ = return "No"

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO [] _ = return []
forIO (x:xs) f = do
    a <- f x
    as <- forIO xs f
    return (a:as)

task5 :: IO ()
task5 = do
    b <- forIO [1, 2, 3] isTwo
    print(b)


-- TASK 6
isTwoNotIO :: Int -> String
isTwoNotIO 2 = "Yes"
isTwoNotIO _ = "No"

mapIO :: (a -> b) -> IO a -> IO b
mapIO f val = do
    temp <- val
    return (f temp)

task6 :: IO ()
task6 = do
    ans <- mapIO isTwoNotIO (return 2)
    print(ans)


-- TASK 7
 

main :: IO ()
main = do
    task6
    

