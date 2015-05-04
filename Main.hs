{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad
import Data.Function (on)
import Data.List (foldl', sortBy)
import Data.Map (Map(..), (!))
import Data.Set (Set(..), (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

type Cell  = (Int, Int)
type Board = Map.Map (Int, Int) [Int]

--------- Utilities ----------

-- | List of all coordinates
coordinates :: [(Int, Int)]
coordinates = liftM2 (,) [0..8] [0..8]

-- | The set of 9 possible cell values
nine :: Set.Set Int
nine = Set.fromList [1..9]

-- | A test board (encoded as row/column/possibility lists)
board' :: [[[Int]]]
board' = (map . map $ \xs -> if null xs then [0..8] else xs) [
  [[ ], [8], [ ], [1], [ ], [ ], [4], [ ], [ ]],
  [[9], [ ], [1], [ ], [ ], [ ], [ ], [ ], [ ]],
  [[ ], [7], [ ], [5], [3], [9], [ ], [ ], [ ]],
  [[8], [1], [ ], [3], [ ], [6], [ ], [2], [4]],
  [[2], [6], [ ], [9], [ ], [7], [ ], [3], [1]],
  [[3], [9], [ ], [4], [ ], [1], [ ], [7], [6]],
  [[ ], [ ], [ ], [7], [1], [5], [ ], [4], [ ]],
  [[ ], [ ], [ ], [ ], [ ], [ ], [7], [ ], [5]],
  [[ ], [ ], [2], [ ], [ ], [4], [ ], [1], [ ]]
  ]

-- | Create a board from a triplist
toBoard :: [[[Int]]] -> Board
toBoard xxs = Map.fromList $ do
  (r, xs) <- zip [0..8] xxs
  (c, x)  <- zip [0..8] xs
  return ((r, c), x)

-- | Test board
board :: Board
board = toBoard board'

-- | Find coordinates of all cells in the same box as a specified cell.
boxCoords :: (Int, Int) -> [(Int, Int)]
boxCoords (i, j) = filter (/= (i, j)) $ map f (liftM2 (,) [0..2] [0..2])
  where
    (i', j') = (i `div` 3, j `div` 3)
    f (r, c) = (i'*3 + r, j'*3 + c)

-- | All coordinates in a row for a given cell (except cell itself)
rowCoords :: (Int, Int) -> [(Int, Int)]
rowCoords (r, c) = map (r,) $ filter (/=c) [0..8]

-- | All coordinates in a column for a given cell (except cell itself)
colCoords :: (Int, Int) -> [(Int, Int)]
colCoords (r, c) = map (,c) $ filter (/=r) [0..8]

-- | Set the value of a cell
set :: (Int, Int) -> Int -> Board -> Board
set ij x = Map.adjust (const [x]) ij

f m ij = case m ! ij of
              x:[] -> [x]
              _    -> []

-- | Remove logical impossibilities from a particular cell
pruneCell :: (Int, Int) -> Board -> [Int]
pruneCell x m = case m ! x of
                     (v:[]) -> [v]
                     vs     -> Set.toList $ nine \\ Set.fromList possible
  where
    possible = concatMap (f m) (colCoords x ++ rowCoords x ++ boxCoords x)

-- | Prune possibilities from all cells on the board
prune board = foldl' next board coordinates
  where next board ij = Map.adjust (const $ pruneCell ij board) ij board

-- | Order keys by number of possibilities
mostConstrained :: Board -> [((Int, Int), [Int])]
mostConstrained b = sortBy (compare `on` (length . snd)) . Map.toAscList $ b

-- | Is a board impossible? (True iff any cell has 0 possibilities)
impossible :: Board -> Bool
impossible = not . null . Map.filter null

-- | Board solved when all cells have 1 solution
solved :: Board -> Bool
solved = null . Map.filter (\xs -> length xs /= 1)


-- | Output all solutions to the board (NOTE: not necessarily unique)
search :: Board -> [Board]
search board
  | impossible board = []
  | solved     board = [board]
  | otherwise        = do
    let pruned = prune board
    (ij, ps) <- mostConstrained pruned
    p        <- ps
    search (set ij p pruned)

-- | Suddenly, python
printBoard board = do
  forM_ [0..8] $ \r -> do
    putStrLn ""
    forM_ [0..8] $ \c -> do
      putStr . show $ board ! (r, c)

  putStrLn ""

-- Print the first solution observed
main = printBoard . head . drop 1 . search $ board
