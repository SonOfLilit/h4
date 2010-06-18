module Computer where
import H4(MoveChooser)
import Board(Player, Board, toColumns, toList, boardHeight, boardWidth)
import Data.Array
import Data.List
import Data.Maybe(fromMaybe, listToMaybe)

data BoardArray = BoardArray [Int] (Array (Int, Int) (Maybe Player))

computerPlayer :: MoveChooser
computerPlayer = ((return .) .) f
  where
    f board player = fromMaybe err (chooseMove player $ toBoardArray board)
    err = error "no more moves"

toBoardArray :: Board -> BoardArray
toBoardArray board = BoardArray (heights board) arr
  where arr = listArray boundaries . concat $ toList board
        boundaries = ((0, 0), (boardWidth-1, boardHeight-1))

chooseMove :: Player -> BoardArray -> Maybe Int
chooseMove player (BoardArray hs board) = findIndex (<boardHeight) hs

heights :: Board -> [Int]
heights = map length . toColumns

(!!++) :: (Num a) => [a] -> Int -> [a]
a !!++ i = h ++ [item + 1] ++ t
  where (h, item : t) = case splitAt i a of
                          (x, y@(_:_)) -> (x, y)
                          _ -> error "Index too large"
