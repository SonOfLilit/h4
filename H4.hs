module H4
     ( MoveChooser
     ) where
import Board

type MoveChooser = Board -> Player -> IO Int
