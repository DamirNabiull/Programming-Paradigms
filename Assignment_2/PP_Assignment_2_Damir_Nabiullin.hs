{-# HLINT ignore "Use const" #-}

-- *****************************************************************************************
-- TASK 1

-- | A line with a focus.
-- Line xs y zs represents a descrete line:
-- * xs represents all elements to the left (below)
-- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a]
    deriving (Show)

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- SUBTASK 1

-- | Keep up to a given number of elements in each direction in a line.
cutLine :: Int -> Line a -> Line a
cutLine n (Line xs y zs) = Line (take n xs) y (take n zs)

-- SUBTASK 2

-- | Apply a function if a given value satisfies the predicate.
applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
applyIf p f x
    | p x       = Just (f x)
    | otherwise = Nothing

-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce
-- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to
-- produce a list of elements to the right of x.

genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (generate f x) x (generate g x)
    where
        generate h y = case h y of
            Just y' -> y' : generate h y'
            Nothing -> []

-- SUBTASK 3

-- | Apply a function to all elements on a line.
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs y zs) = Line (map f xs) (f y) (map f zs)

-- SUBTASK 4

-- | Zip together two lines.
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xs y zs) (Line xs' y' zs') = Line (zip xs xs') (y, y') (zip zs zs')

-- | Zip together two lines with a given combining function.
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line xs y zs) (Line xs' y' zs') = Line (zipWith f xs xs') (f y y') (zipWith f zs zs')



-- *****************************************************************************************
-- TASK 2

-- SUBTASK 1

data Cell = Alive | Dead
    deriving (Show)

rule30 :: Line Cell -> Cell
rule30 (Line (a:_) b []) = case (a, b) of
    (Alive, Alive) -> Dead
    (Alive, Dead)  -> Alive
    (Dead, Alive)  -> Alive
    (Dead, Dead)   -> Dead
rule30 (Line [] b (c:_)) = case (b, c) of
    (Alive, Alive) -> Alive
    (Alive, Dead)  -> Alive
    (Dead,  Alive) -> Alive
    (Dead,  Dead)  -> Dead
rule30 (Line (a:_) b (c:_)) = case (a, b, c) of
    (Alive, Alive, Alive) -> Dead
    (Alive, Alive, Dead)  -> Dead
    (Alive, Dead,  Alive) -> Dead
    (Alive, Dead,  Dead)  -> Alive
    (Dead,  Alive, Alive) -> Alive
    (Dead,  Alive, Dead)  -> Alive
    (Dead,  Dead,  Alive) -> Alive
    (Dead,  Dead,  Dead)  -> Dead

-- SUBTASK 2

shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing
shiftLeft (Line (x:xs) y zs) = Just (Line xs x (y:zs))

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing
shiftRight (Line xs y (z:zs)) = Just (Line (y:xs) z zs)

-- SUBTASK 3

lineShifts :: Line a -> Line (Line a)
lineShifts l = genLine shiftLeft l shiftRight

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- SUBTASK 4


main :: IO()
main = do
    putStrLn "TASK 1"
    putStrLn "SUBTASK 1"
    print (cutLine 3 integers)
    putStrLn "SUBTASK 2"
    print (genLine (applyIf (> -3) (subtract 1)) 0 (applyIf (< 3) (+1)))
    print (genLine (\_ -> Nothing) 1 (\_ -> Nothing))
    print (cutLine 3 (genLine Just 0 Just))
    putStrLn "SUBTASK 3"
    print (cutLine 3 (mapLine (^2) integers))
    putStrLn "SUBTASK 4"
    print (cutLine 3 (zipLines integers integers))
    print (cutLine 3 (zipLinesWith (*) integers integers))

    putStrLn "TASK 2"
    print (applyRule30 (Line [Alive, Dead] Alive [Alive, Dead]))