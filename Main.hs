module Main where
import Data.List

data Player = Player1
            | Player2

class BoardClass a where
    width, height :: a -> Int
    width = const 9
    height = const 9
    toColumns :: a -> [[Player]]
    toList :: a -> [[Maybe Player]]
    toList board = transpose . fill $ toColumns board
        where fill columns = map fillColumn columns
              fillColumn col = replicate k Nothing ++ map Just col
	          where k = width board - length col
    newBoard :: a

data Board = Board [[Player]]
instance BoardClass Board where
    toColumns (Board columns) = columns
    newBoard = Board $ replicate (width (Board [[]])) []

instance Show Player where show Player1 = "0"
                           show Player2 = "@"

instance Show Board where show=showBoard

showBoard board = unlines . map (unwords . map (maybe " " show)) $ toList board

other :: Player -> Player
other Player1 = Player2
other Player2 = Player1

main :: IO ()
main = do
    putStrLn "Hello and welcome"
    handleRound newBoard Player1

handleRound :: Board -> Player -> IO ()
handleRound board player = do
    putStr "Board position:"
    putStr $ show board
    putStrLn $ show player ++ ", choose a column:"
    line <- getLine
    let column = read line :: Int
    maybe win (`handleRound` other player) (move player board column)
        where win = putStrLn "You win!"

move :: Player -> Board -> Int -> Maybe Board
move player (Board columns) i = Just . Board $ (h ++ ((player : col) : t))
    where h, t :: [[Player]]
          col :: [Player]
          (h, (col : t)) = splitAt i columns
