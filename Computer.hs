module Computer where
import H4(MoveChooser)
import Board(toColumns, boardHeight)
import Data.List(findIndex)
import Data.Maybe(fromMaybe)

computerPlayer :: MoveChooser
computerPlayer = ((return .) .) f
  where
    f board _ = let columns = toColumns board
                in fromMaybe (error "no more moves") 
                             (findIndex ((< boardHeight) . length) columns)
