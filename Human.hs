module Human where
import H4(MoveChooser)

humanPlayer :: MoveChooser
humanPlayer board player = do     
  line <- getLine
  return $ read line - 1
