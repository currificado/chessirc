module Game where

{-

Definición del datatype Game

-}

import Input

import Chess
import Chess.FEN
import Chess.PGN

import Data.Array
import Data.List
import Data.Maybe
import Data.Time


type MoveHistory = [(Int, SANMove, Maybe SANMove)]

data Game = Game { site            :: Channel
                 , date            :: Day
                 , white           :: Maybe Nick
                 , black           :: Maybe Nick
                 , initialPosition :: Maybe Board
                 , board           :: Maybe Board
                 , drawoffer       :: Bool
                 , history         :: MoveHistory
                 , result          :: Maybe GameResult
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
    
translatePiece :: Char -> Char
translatePiece 'K' = 'R'
translatePiece 'Q' = 'D'
translatePiece 'R' = 'T'
translatePiece 'B' = 'A'
translatePiece 'N' = 'C'
translatePiece ch = ch

translate :: SANMove -> SANMove
translate = map translatePiece
