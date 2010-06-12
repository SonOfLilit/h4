module Main where
import Data.List

data Player = Player1
            | Player2
    deriving Eq

instance Show Player where show Player1 = "0"
                           show Player2 = "@"

boardWidth, boardHeight :: Int
boardWidth = 9
boardHeight = 6

other :: Player -> Player
other Player1 = Player2
other Player2 = Player1

class BoardClass a where
    toColumns :: a -> [[Player]]
    toList :: a -> [[Maybe Player]]
    toList board = transpose . fill $ toColumns board
        where fill columns = map fillColumn columns
              fillColumn col = replicate k Nothing ++ map Just col
	          where k = boardHeight - length col
    newBoard :: a

data Board = Board [[Player]]
instance BoardClass Board where
    toColumns (Board columns) = columns
    newBoard = Board $ replicate boardWidth []

instance Show Board where show=showBoard

showBoard board = unlines . map (unwords . map (maybe " " show)) $ toList board

main :: IO ()
main = do
    putStrLn "Hello and welcome"
    handleRound newBoard Player1

handleRound :: Board -> Player -> IO ()
handleRound board player = do
    putStrLn "Board position:"
    putStr $ show board
    putStrLn . unwords . map show $ [1..9]
    putStrLn $ show player ++ ", choose a column:"
    line <- getLine
    let column = (read line - 1) :: Int
    maybe win (`handleRound` other player) (move player board column)
        where win = putStrLn "You win!"

move :: Player -> Board -> Int -> Maybe Board
move player board@(Board columns) x =
    errorIf illegal "Illegal Move" $ justIf (not winningMove) updatedBoard
        where
            illegal = (x < 0) || (x >= boardWidth) || (y == boardHeight)

            winningMove = or . map makesRow $ [[d], [l, r], [ul, dr], [dl, ur]]
            d = count 0 (-1)
            l = count (-1) 0
            r = count 1 0
            ul = count (-1) 1
            dr = count 1 (-1)
            dl = count (-1) (-1)
            ur = count 1 1
            count = countInDirection player columns x y
            makesRow rowLengths = sum rowLengths >= (4-1)

            updatedBoard = Board $ (h ++ ((player : col) : t))

            (h, (col : t)) = splitAt x columns
            y = length col

countInDirection player board x0 y0 dx dy =
    let x = x0+dx
        y = y0+dy
    in
        if (exists board x y) && (at board x y == player)
        then 1 + countInDirection player board x y dx dy
        else 0

exists board i j =
    (i >= 0) && (i < boardWidth) &&
    (j >= 0) && (j < boardHeight) &&
    (length (board !! i) > j)

at board i j = col !! (length col - 1 - j)
    where col = board !! i

errorIf :: Bool -> String -> a -> a
errorIf cond message value = if cond then error message else value

justIf :: Bool -> a -> Maybe a
justIf cond value = if cond then Just value else Nothing
