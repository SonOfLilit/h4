module Computer where
import H4(MoveChooser)
import Board(Player, Board, toColumns, toList, boardHeight, boardWidth)
import Data.Array
import Data.List
import Data.Maybe(catMaybes, fromJust, listToMaybe)

data BoardState = BoardState Player [Int] (Array (Int, Int) (Maybe Player))
    deriving Show

computerPlayer :: MoveChooser
computerPlayer = ((return .) .) f
  where
    f board player = fromJust (chooseMove $ toBoardState board player)

toBoardState :: Board -> Player -> BoardState
toBoardState board player = BoardState player (heights board) arr
  where arr = listArray boundaries . concat $ toList board
        boundaries = ((0, 0), (boardWidth-1, boardHeight-1))

heights :: Board -> [Int]
heights = map length . toColumns

chooseMove :: BoardState -> Maybe Int
chooseMove board = listToMaybe $ catMaybes [winning, legal]
  where winning = find (moveWins board) legalMoves'
        legal = listToMaybe legalMoves'
        legalMoves' = legalMoves board

legalMoves :: BoardState -> [Int]
legalMoves (BoardState _ hs _) = findIndices (<boardHeight) hs

moveWins :: BoardState -> Int -> Bool
moveWins (BoardState player hs arr) x =
    any (fourRowOf player) $ potentialRows arr' x y
  where y = hs !! x
        arr' = arr // [((x, y), Just player)]

potentialRows :: Array (Int, Int) (Maybe Player) -> Int -> Int -> [[Maybe Player]]
potentialRows arr x y = map (map (arr!)) $ [row, col, diagUp, diagDown]
  where row = [(i, y) | i <- [0..xMax]]
        col = [(x, i) | i <- [0..yMax]]
        diagUp = [(i, j) | i <- [boundX (x-y) .. boundX (yMax-(y-x))], let j = (y-x)+i]
        diagDown = [(i, j) | i <- [boundX (x+y-yMax) .. boundX (x+y)], let j = (x+y)-i]
        boundX i = max 0 $ min xMax i
        ((0,0), (xMax, yMax)) = bounds arr

fourRowOf :: Player -> [Maybe Player] -> Bool
fourRowOf player row = foldr f 0 row == (4 :: Int)
  where f _ 4 = 4       -- once found 4, stop resetting
        f (Just p) k | p == player = k+1
        f _ _ = 0       -- reset count

(!!++) :: (Num a) => [a] -> Int -> [a]
a !!++ i = h ++ [item + 1] ++ t
  where (h, item : t) = case splitAt i a of
                          (x, y@(_:_)) -> (x, y)
                          _ -> error "Index too large"
