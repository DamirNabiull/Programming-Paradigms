{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Replace case with maybe" #-}

import Data.Char (toUpper)
import Text.Read (readMaybe)

-- TASK 1

-- guess p g = do
--     s <- getLine
--     x <- g s
--     case p x of
--         True -> return x
--         False -> guess p g
-- 
-- Let assume that value x has type 't'
-- Then, from 
--   True -> return x
-- we get that guess returns IO t
-- 
-- From 
--   case p x of
--     True -> return x
-- we get that p of type t -> Bool
-- 
-- Moreover, we know that s is String
-- And, g is a function of type String -> IO t
-- 
-- Now we can find the full type of guess:
--   (t -> Bool) -> (String -> IO t) -> IO t


-- TASK 2

echo :: IO ()
echo = do
    s <- getLine
    putStrLn (map toUpper s)
    echo

-- TASK 3
-- A

foreverIO :: IO a -> IO b
foreverIO action = do
    action
    foreverIO action

-- B
whenIO :: Bool -> IO () -> IO ()
whenIO True action = do
    action
whenIO _ _ = do
    return ()

-- C
maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO Nothing = do
    return Nothing
maybeIO (Just action) = do
    act <- action
    return (Just act)
    
-- D
sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO [] = return []
sequenceMaybeIO (x:xs) = do
    res <- x
    others <- sequenceMaybeIO xs
    case res of
        Just temp -> return (temp:others)
        Nothing -> return others

-- E
parseIntValue :: Int -> IO (Maybe Int)
parseIntValue 0 = do
    putStrLn "Nothing"
    return Nothing
parseIntValue val = do
    print val
    return (Just (val - 1))

whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO func val = do
    res <- func val
    case res of
        Nothing -> return ()
        Just next ->  whileJustIO func next

-- F
verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
    putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
    return (x:xs)

forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ state [] _ = return state
forStateIO_ state (x:xs) func = do
    new_state <- func x state
    forStateIO_ new_state xs func


-- TASK 4
iforIO_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_ [] _ = return ()
iforIO_ lst func = next 0 lst
    where
        next _ [] = return ()
        next i (x:xs) = do
            func i x
            next (i + 1) xs


example = do
  iforIO_ [1, 2] (\i n ->
    iforIO_ "ab" (\j c ->
      print ((i, j), replicate n c)))

        

main :: IO ()
main = do
    putStrLn "\n\n3 B"
    whenIO True (print "True")
    whenIO False (print "False")

    putStrLn "\n\n3 C"
    a <- maybeIO (Just (putStrLn "#####")) 
    b <- maybeIO Nothing 

    putStrLn "\n\n3 D"
    c <- sequenceMaybeIO [return (Just 5), return (Just 4), return Nothing, return (Just 3)]
    print c

    putStrLn "\n\n3 E"
    whileJustIO parseIntValue 2

    putStrLn "\n\n3 F"
    s <- forStateIO_ [] [1, 2, 3] verboseCons
    print s

    putStrLn "\n\nTask 4"
    example