module Main where
import Board(Player(..), other, Board, newBoard, move)
import H4(MoveChooser)
import Human(humanPlayer)
import Computer(computerPlayer)

main :: IO ()
main = do
    putStrLn "Hello and welcome"
    handleRound (humanPlayer, computerPlayer) newBoard Player1

-- TODO: handle draw
-- TODO: Talk to flatmates about garbage with holes and garbage that is big
handleRound :: (MoveChooser, MoveChooser) -> Board -> Player -> IO ()
handleRound (thisA, otherA) board player = do
  putStrLn "Board position:"
  print board
  putStrLn $ show player ++ ", choose a column:"
  column <- thisA board player
  putStrLn $ show (column + 1) ++ ", wise choice!"
  maybe win handleNextRound (move player board column)
    where win = putStrLn $ show player ++ " wins!"
          handleNextRound b = handleRound (otherA, thisA) b $ other player
