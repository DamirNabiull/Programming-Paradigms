import CodeWorld

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wall #-}

-- *****************************************************************************************
-- TASK 1

{- | A line with a focus.
   Line xs y zs represents a descrete line:
   * xs represents all elements to the left (below)
   * y is the element in focus
   * zs represents all elements after (above)
-}
data Line a = Line [a] a [a]
    deriving (Show)

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- SUBTASK 1.1

{- | Keep up to a given number of elements in each direction in a line.
   using take function create new line of n elements from left and right
-}
cutLine :: Int -> Line a -> Line a
cutLine n (Line xs y zs) = Line (take n xs) y (take n zs)

-- SUBTASK 1.2

-- | Apply a function if a given value satisfies the predicate.
applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
applyIf p f x
    | p x       = Just (f x)
    | otherwise = Nothing

{- | Generate a line by using generating functions.
   (genLine f x g) generates a line with x in its focus,
   then it applies f to x until reaching Nothing to produce
   a list of elements to the left of x,
   and, similarly, applies g to x until reaching Nothing to
   produce a list of elements to the right of x.
-}
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (generate f x) x (generate g x)
    where
        generate h y = case h y of
            Just y' -> y' : generate h y'
            Nothing -> []

-- SUBTASK 1.3

{- | Apply a function to all elements on a line.
   map through left and right parts of line and 
   apply function to each element of line
-}
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs y zs) = Line left center right
    where
        left   = map f xs
        center = f y
        right  = map f zs

-- SUBTASK 1.4

{- | Zip together two lines.
   zips left and right parts of line using zip
   and make focus as tupple of focus of both lines
-}
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xs y zs) (Line xs' y' zs') = Line left center right
    where
        left   = zip xs xs'
        center = (y, y')
        right  = zip zs zs'

{- | Zip together two lines with a given combining function.
    zips left and right parts of line using zipWith
   and make focus as result of applied function to focus of both lines
-}
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line xs y zs) (Line xs' y' zs') = Line left center right
    where
        left   = zipWith f xs xs'
        center = f y y'
        right  = zipWith f zs zs'



-- *****************************************************************************************
-- TASK 2

-- SUBTASK 1.5

-- | A life cell.
data Cell = Alive | Dead
    deriving (Show)

-- | Compute the next state of the cell in focus, according to Rule 30
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

-- SUBTASK 1.6

-- | Shift focus of line to left.
forceShiftLeft :: Line a -> Line a
forceShiftLeft (Line (x:xs) y zs) = Line xs x (y:zs)

-- | Shift focus of line to left if possible.
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing
shiftLeft line          = Just (forceShiftLeft line)

-- | Shift focus of line to right.
forceShiftRight :: Line a -> Line a
forceShiftRight (Line xs y (z:zs)) = Line (y:xs) z zs

-- | Shift focus of line to right if possible.
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing
shiftRight line          = Just (forceShiftRight line)

-- SUBTASK 1.7

{- | Maps line to all possible focus shifts.
   Creates line of all possible shifts of line
   by generating new line with shifted focus
-}
lineShifts :: Line a -> Line (Line a)
lineShifts l = genLine shiftLeft l shiftRight

-- | Calculates the next state of the complete line using rule30.
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- SUBTASK 1.8

{- | Renders picture from list of pictures with given padding.
   Combine each picture inside list with previous pictures
   and translate next picture on horizontal axis by padding
-}
renderPart :: [Picture] -> Double -> Picture
renderPart []     _ = blank
renderPart (x:xs) y = first <> other
    where
        first = translated y 0 x
        other = translated y 0 (renderPart xs y)

-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line xs y zs) = left <> y <> right
  where
    left  = renderPart xs (-1.05)
    right = renderPart zs 1.05

-- | Example for subtask 1.8.
sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)

{- | Render cell as 1x1 rectangle.
   Alive = black rectangle, Dead = white rectangle
-}
renderCell :: Cell -> Picture
renderCell Alive = solidRectangle  1 1
renderCell Dead  = rectangle  1 1

{- | Create line of pictures from line of cells.
   Goes through each part of line and render each cell
   using renderCell function
-}
renderLineCells :: Line Cell -> Line Picture
renderLineCells (Line xs y zs) = Line left center right
  where
    left   = map renderCell xs
    center = renderCell y
    right  = map renderCell zs

{- | Render the fist N steps of Rule 30,
   applied to a given starting line.
   It renders each line of cells as a line of pictures
   using renderLineCells function
   and then render all lines of pictures
   using renderLine function.
   Translates each new line on vertical axis by padding.
-}
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 _    = blank
renderRule30 n line = first <> other
    where
        first = renderLine (renderLineCells line)
        other = translated 0 (-1.05) (renderRule30 (n-1) (applyRule30 line))



-- *****************************************************************************************
-- TASK 3

{- | A descrete 2D space with a focus.
   A 2D space is merely a (vertical) line
   where each element is a (horizontal) line.
-}
data Space a = Space (Line (Line a))
    deriving (Show) 
    
-- SUBTASK 1.9

-- | Create a space from a lines elements.
makeSpace :: [Line a] -> Line a -> [Line a] -> Space a
makeSpace xs y zs = Space (Line xs y zs)

{- | Get line of all possible combinations of value and line. 
   Zips value with each line element and creates new line
-}
getCombinations :: Line a -> b -> Line (b, a)
getCombinations (Line xs y zs) b = Line left (b, y) right
    where
        left  = zip (repeat b) xs
        right = zip (repeat b) zs

{- | Generate a space by using generating functions.
   Create space from two given lines according
   to multiplication rule of cartesian product
   represented as getCombinations function.
-}
productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines (Line xs y zs) line = makeSpace left center right
    where
        left   = map (getCombinations line) xs
        center = getCombinations line y
        right  = map (getCombinations line) zs

-- SUBTASK 1.10

{- | Apply a function to all elements on a space
   Maps through each line of space, then
   maps through each line of line and applies function
-}
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line xs y zs)) = makeSpace left center right
    where
        left   = map (mapLine f) xs
        center = mapLine f y
        right  = map (mapLine f) zs

{- | Zip together two spaces.
   Zips each line of space with each line of other space
   and creates new space.
-}
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line xs y zs)) (Space (Line xs' y' zs')) 
    = makeSpace left center right
    where
        left   = zipWith zipLines xs xs'
        center = zipLines y y'
        right  = zipWith zipLines zs zs'

{- | Zip together two spaces with a given combining function.
   Zips each line of space with each line of other space
   usinf given function and creates new space.
-}
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f (Space (Line xs y zs)) (Space (Line xs' y' zs')) 
    = makeSpace left center right
    where
        left   = zipWith (zipLinesWith f) xs xs'
        center = zipLinesWith f y y'
        right  = zipWith (zipLinesWith f) zs zs'

-- SUBTASK 1.11
-- Completed in SUBTASK 1.10


-- *****************************************************************************************
-- TASK 4

-- SUBTASK 1.12

{- | Calculates all alive neighbours of foci.
   Gets all neighbours of foci and counts
   how many of them are alive using tryCountLine
   and tryCountCell function.
-}
calculateAliveNeighbours :: Line (Line Cell) -> Int
calculateAliveNeighbours (Line xs (Line left _ right) zs) = 
    tryCountLine xs
    + tryCountLine zs
    + tryCountCell left
    + tryCountCell right

-- | Tries to count all alive neighbours of first line in list.
tryCountLine :: [Line Cell] -> Int
tryCountLine []     = 0
tryCountLine (x:xs) = countLine x

-- | Counts alive cells in a line.
countLine :: Line Cell -> Int
countLine (Line xs y zs) = tryCountCell xs + countCell y + tryCountCell zs

-- | Tries to count whether first cell in list is alive.
tryCountCell :: [Cell] -> Int
tryCountCell []     = 0
tryCountCell (x:xs) = countCell x

-- | Counts if cell is alive.
countCell :: Cell -> Int
countCell Alive = 1
countCell _     = 0

{- | Calculates the next state of foci of space after apllying conway's rule.
   Calculates alive neighbours of foci and
   checks conway's rule.
-}
conwayRule :: Space Cell -> Cell
conwayRule (Space (Line xs (Line left center right) zs)) = 
    case calculateAliveNeighbours (Line xs (Line left center right) zs) of
        2 -> center
        3 -> Alive
        _ -> Dead

-- SUBTASK 1.13

-- | Example conway's space.
exampleConwaySpace = 
    Space (Line [Line [Dead, Dead] Alive [Dead, Dead], 
                 Line [Dead, Dead] Dead [Dead, Dead]] 
                (Line [Dead, Dead] Dead [Alive, Dead]) 
                [Line [Alive, Dead] Alive [Alive, Dead], 
                 Line [Dead, Dead] Dead [Dead, Dead]])

{- | Shift space to the left.
   Shifts each line of space to the left
   using shiftLineLeft function if possible.
-}
spaceShiftLeft :: Line (Line a) -> Maybe (Line (Line a))
spaceShiftLeft (Line xs y zs) =
    case shiftLeft y of
        Nothing -> Nothing
        Just v  -> Just (mapLine forceShiftLeft (Line xs y zs))

{- | Shift space to the right.
   Shifts each line of space to the right
   using shiftLineRight function if possible.
-}
spaceShiftRight :: Line (Line a) -> Maybe (Line (Line a))
spaceShiftRight (Line xs y zs) =
    case shiftRight y of
        Nothing -> Nothing
        Just v  -> Just (mapLine forceShiftRight (Line xs y zs))

{- | Maps space to all possible focus shifts.
   Creates space of all possible shifts of a given space
   by generating new space with shifted space focus
-}
spaceShifts :: Space a -> Space (Space a)
spaceShifts (Space line) = Space (mapLine spaceShift (lineShifts line))
    where
        spaceShift line = 
            mapLine Space 
                    (genLine spaceShiftLeft line spaceShiftRight)
 
-- | Calculates next state of space by appling conway's rule.
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- SUBTASK 1.14

-- | Renders a space of cells as space of pictures.
convertCellSpace :: Space Cell -> Space Picture
convertCellSpace (Space line) = Space (mapLine renderLineCells line)

-- | Renders lines one under another.
renderVertical :: [Line Picture] -> Picture
renderVertical []     = blank
renderVertical (x:xs) = first <> other
    where
        first = renderLine x
        other = translated 0 (-1.05) (renderVertical xs)

-- | Render a space of 1x1 pictures.
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line xs y zs)) = renderVertical (xs ++ [y] ++ zs)

-- | Type that represents state of animation.
type State = (Double, Space Cell)

-- | Handles time change and creates new state.
handleState :: Event -> State -> State
handleState (TimePassing dt) (t, space)
    | t + dt >= 1 = (0, applyConwayRule space)
    | otherwise   = (t + dt, space)
handleState _ state = state

-- | Renders state of animation.
renderState :: State -> Picture
renderState (_, space) = renderSpace (convertCellSpace space)

{- | Animate Conway's Game of Life,
   starting with a given space
   and updating it every second.
-}
animateConway :: Space Cell -> IO ()
animateConway space = debugActivityOf (0, space) handleState renderState


main :: IO()
main = do
    putStrLn "\n\nTASK 1"
    putStrLn "\nSUBTASK 1.1"
    print (cutLine 3 integers)
    putStrLn "\nSUBTASK 1.2"
    print (genLine (applyIf (> -3) (subtract 1)) 0 (applyIf (< 3) (+1)))
    print (genLine (\_ -> Nothing) 1 (\_ -> Nothing))
    print (cutLine 3 (genLine Just 0 Just))
    putStrLn "\nSUBTASK 1.3"
    print (cutLine 3 (mapLine (^2) integers))
    putStrLn "\nSUBTASK 1.4"
    print (cutLine 3 (zipLines integers integers))
    print (cutLine 3 (zipLinesWith (*) integers integers))

    putStrLn "\n\nTASK 2"
    putStrLn "\nSUBTASK 1.5"
    print (rule30 (Line [Dead, Alive, Alive] 
                        Alive 
                        [Alive, Alive, Alive]))

    print (rule30 (Line [Alive, Alive, Alive] 
                        Alive 
                        [Alive, Alive, Alive]))

    putStrLn "\nSUBTASK 1.6"
    print (shiftRight (Line [0,1,1] 1 [1,1,1]))
    print (shiftLeft (Line [0,1,1] 1 [1,1,1]))
    putStrLn "\nSUBTASK 1.7"
    print (lineShifts (Line [2,1] 3 [4,5]))
    print (cutLine 4 $ applyRule30 (Line ([Dead, Alive, Alive] ++ repeat Dead) 
                                          Alive 
                                         ([Alive, Alive, Alive] ++ repeat Dead)))
    putStrLn "\nSUBTASK 1.8"
    putStrLn "Uncomment the following lines to see the result of subtask 1.8"
    -- drawingOf (renderLine sampleLine)
    -- drawingOf (renderRule30 7 (Line [Dead,Dead,Dead,Dead,Dead] 
    --                                 Alive 
    --                                 [Dead,Dead,Dead,Dead,Dead]))
    
    putStrLn "\n\nTask 3"
    putStrLn "\nSUBTASK 1.9"
    print (productOfLines (cutLine 3 integers) (cutLine 2 integers))
    putStrLn "\nSUBTASK 1.10"
    print(mapSpace (^2) (Space 
                            (Line [Line [1] 2 [3]] 
                                  (Line [1] 2 [3]) 
                                  [Line [1] 2 [3]])))

    print(zipSpaces (Space 
                        (Line [Line [1] 2 [3]] 
                              (Line [1] 2 [3]) 
                              [Line [1] 2 [3]])) 
                    (Space 
                        (Line [Line [1] 4 [9]] 
                              (Line [1] 4 [9]) 
                              [Line [1] 4 [9]])))

    print(zipSpacesWith (*) (Space 
                                (Line [Line [1] 2 [3]] 
                                      (Line [1] 2 [3]) 
                                      [Line [1] 2 [3]])) 
                            (Space 
                                (Line [Line [1] 4 [9]] 
                                      (Line [1] 4 [9]) 
                                      [Line [1] 4 [9]])))
    putStrLn "\nSUBTASK 1.11"
    putStrLn "Completed in SUBTASK 1.10"

    putStrLn "\n\nTask 4"
    putStrLn "\nSUBTASK 1.12"
    print(conwayRule exampleConwaySpace)

    putStrLn "\nSUBTASK 1.13"
    print "You can see the result of subtask 1.13 in the picture/animation in subtask 1.14"
    
    putStrLn "\nSUBTASK 1.14"
    animateConway exampleConwaySpace