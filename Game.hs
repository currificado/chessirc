module Game where

import Input

import Chess
import Chess.FEN

import Data.Array
import Data.List
import Data.Maybe
import Data.Time

type Date    = String
data Result  = WhiteWon
             | BlackWon
             | Draw
             deriving Eq

data Game = Game { site    :: Channel
                 , date    :: Date
                 , white   :: Maybe Nick
                 , black   :: Maybe Nick
                 , board   :: Board
                 , history :: [(Int, SANMove, Maybe SANMove)]
                 , result  :: Maybe Result
                 } deriving Eq

stringifyPiece :: Maybe Piece -> String
stringifyPiece Nothing = "   "
stringifyPiece (Just (Piece c t)) = fg c ++ " " ++ tx t ++ " "
    where fg White  = "<bWHITE>"
          fg Black  = "<bBLACK>"
          tx Rook   = "R"
          tx Knight = "N"
          tx Bishop = "B"
          tx Queen  = "Q"
          tx King   = "K"
          tx Pawn   = "P"

stringifySquare :: Color -> Maybe Piece -> String
stringifySquare c p = bg c ++ stringifyPiece p
    where bg White = "<BYELLOW>"
          bg Black = "<BBLUE>"
                   

stringifyBoard :: Color -> Board -> String
stringifyBoard perpective brd =
    unlines ( [show (y+1) ++ " " 
               ++ concat([stringifySquare (color (x,y)) (Chess.board brd ! (x,y)) | x<-xlist])
               ++ "<NORMAL>" | y<-ylist] ++ ["   " ++ intercalate "  " (map (\c -> [c]) columns)] )
    where color (x,y) = if odd (x+y) then White else Black
          xlist       = if perpective == White then [0..7] else [7,6..0]
          ylist       = if perpective == White then [7,6..0] else [0..7]
          columns     = if perpective == White then ['a'..'h'] else ['h','g'..'a']
    