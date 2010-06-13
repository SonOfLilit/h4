module Main where
import Board(Player(..), other, Board, newBoard, move)

main :: IO ()
main = do
    putStrLn "Hello and welcome"
    handleRound newBoard Player1

handleRound :: Board -> Player -> IO ()
handleRound board player = do
    putStrLn "Board position:"
    putStrLn $ show board
    putStrLn $ show player ++ ", choose a column:"
    line <- getLine
    let column = (read line - 1) :: Int
    maybe win (`handleRound` other player) (move player board column)
        where win = putStrLn "You win!"
