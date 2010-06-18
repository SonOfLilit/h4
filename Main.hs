module Main where
import Board(Player(..), other, toColumns, Board, boardHeight, newBoard, move)
import Data.List(findIndex)
import Data.Maybe(fromMaybe)
import Monad(liftM2)

type Algorithm = Board -> Player -> IO Int

main :: IO ()
main = do
    putStrLn "Hello and welcome"
    handleRound (humanPlayer, computerPlayer) newBoard Player1

handleRound :: (Algorithm, Algorithm) -> Board -> Player -> IO ()
handleRound (thisA, otherA) board player = do
  column <- thisA board player
  maybe win handleNextRound (move player board column)
    where win = putStrLn "You win!"
          handleNextRound b = handleRound (otherA, thisA) b $ other player

humanPlayer :: Algorithm
humanPlayer board player = do     
  putStrLn "Board position:"
  print board
  putStrLn $ show player ++ ", choose a column:"
  line <- getLine
  return $ read line - 1

computerPlayer :: Algorithm
computerPlayer = ((return .) .) f
  where
    f :: Board -> Player -> Int
    f board _ = let columns = toColumns board
                in fromMaybe (error "no more moves") 
                             (findIndex ((< boardHeight) . length) columns)
