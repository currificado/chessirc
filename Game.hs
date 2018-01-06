module Game where

import Input

import Chess
import Chess.FEN

import Data.Array
import Data.List
import Data.Maybe
import Data.Time

data Result  = WhiteWon
             | BlackWon
             | Draw
             deriving Eq
             
instance Show Result where
    show WhiteWon  = "Blancas ganan. 1-0"
    show BlackWon  = "Negras ganan. 0-1"
    show Game.Draw = "Tablas. ½-½"

type MoveHistory = [(Int, SANMove, Maybe SANMove)]

data Game = Game { site      :: Channel
                 , date      :: Day
                 , drawOffer :: Bool
                 , white     :: Maybe Nick
                 , black     :: Maybe Nick
                 , board     :: Board
                 , history   :: MoveHistory
                 , result    :: Maybe Result
                 } deriving Eq

stringifyPiece :: Maybe Piece -> String
stringifyPiece Nothing = "   "
stringifyPiece (Just (Piece c t)) = fg c ++ " " ++ tx t ++ " "
    where fg White  = "<bWHITE>"
          fg Black  = "<bBLACK>"
          tx Rook   = "♜"
          tx Knight = "♞"
          tx Bishop = "♝"
          tx Queen  = "♛"
          tx King   = "♚"
          tx Pawn   = "♟"

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
    
