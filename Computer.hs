module Computer where
import H4(MoveChooser)
import Board(Player, other, Board, toColumns, toList, boardHeight, boardWidth)
import Data.Array
import Data.List
import Data.Function(on)
import Data.Maybe(catMaybes, fromJust, listToMaybe)

data Score = Lose
           | Undecided
           | Win
             deriving (Eq, Ord, Show)

data BoardState = BoardState Player [Int] (Array (Int, Int) (Maybe Player)) Int
    deriving Show

computerPlayer :: MoveChooser
computerPlayer = ((return .) .) f
  where
    f board player = fromJust (chooseMove $ toBoardState board player)

toBoardState :: Board -> Player -> BoardState
toBoardState board player = BoardState player (heights board) arr (-1)
  where arr = listArray boundaries . concat $ toList board
        boundaries = ((0, 0), (boardWidth-1, boardHeight-1))

lastMove :: BoardState -> Int
lastMove (BoardState _ _ _ l) = l

heights :: Board -> [Int]
heights = map length . toColumns

chooseMove :: BoardState -> Maybe Int
chooseMove board = getBestMove 4 board >>= Just . snd

getBestMove :: Int -> BoardState -> Maybe (Score, Int)
getBestMove k = listToMaybe . generateSortedMoves k

generateSortedMoves :: Int -> BoardState -> [(Score, Int)]
generateSortedMoves k board = sortBy compareScores . catMaybes $ results 
  where roundResults = [move' board i | i <- [0..boardWidth-1]]
        results :: [Maybe (Score, Int)]
        results = map (>>= format) roundResults
        format (sc, st) = Just (sc, lastMove st)
        move' = if k == 0 then move else moveRecursive (k-1)
        compareScores = flip (compare `on` fst)

moveRecursive :: Int -> BoardState -> Int -> Maybe (Score, BoardState)
moveRecursive k board x =
    case move board x of
      Just mv@(Undecided, board') ->
          getBestMove k board' >>= reverseScore mv
      e -> e

reverseScore :: (Score, a) -> (Score, b) -> Maybe (Score, a)
reverseScore firstMove secondMove = let (_, b) = firstMove
                                        (score, _) = secondMove
                                    in case score of
                                         Win -> Just (Lose, b)
                                         Lose -> Just (Win, b)
                                         _else -> Just firstMove

move :: BoardState -> Int -> Maybe (Score, BoardState)
move (BoardState player hs arr _) x
    | y < boardHeight = Just (score, board')
    | otherwise = Nothing
  where score = scoreLastMove board'
        board' = BoardState player' hs' arr' x
        player' = other player
        hs' = hs !!++ x
        y = hs !! x
        arr' = arr // [((x, y), Just player)]

-- Haskell is lazy, therefore it is ok to use this even when a numeric
-- score will be attached to Undecided
--
-- NOTE: Passing y and player might improve performance
scoreLastMove :: BoardState -> Score
scoreLastMove (BoardState player hs arr x) =
    -- the state says which player is *next*
    if any (fourRowOf $ other player) $ potentialRows arr x y
    then Win
    else Undecided
  where
        -- the state says how high the *next* move will be
        y = (hs !! x) - 1

potentialRows :: Array (Int, Int) (Maybe Player) -> Int -> Int -> [[Maybe Player]]
potentialRows arr x y = map (map (arr!)) [row, col, diagUp, diagDown]
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
