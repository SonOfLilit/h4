module main where
import Board(Player(..), other, Board, newBoard, move)
import H4(MoveChooser)
import Human(humanPlayer)
import Computer(computerPlayer)

main :: IO ()
main = do
    putStrLn "Hello and welcome"
    handleRound (humanPlayer, computerPlayer) newBoard Player1

handleRound :: (MoveChooser, MoveChooser) -> Board -> Player -> IO ()
handleRound (thisA, otherA) board player = do
  column <- thisA board player
  maybe win handleNextRound (move player board column)
    where win = putStrLn "You win!"
          handleNextRound b = handleRound (otherA, thisA) b $ other player
