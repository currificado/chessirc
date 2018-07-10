module Game where

{-

Definición del datatype Game

-}

import Data.Time.Calendar
import System.IO

import Input

import Chess
import Chess.FEN
import Chess.PGN as P

import Data.Array
import Data.List
import Data.Maybe
import Data.Time


type MoveHistory = [(Int, SANMove, Maybe SANMove)]

data Game = Game { site            :: Channel
                 , date            :: UTCTime
                 , white           :: Maybe Nick
                 , black           :: Maybe Nick
                 , initialPosition :: Maybe Board
                 , board           :: Maybe Board
                 , drawoffer       :: Bool
                 , history         :: MoveHistory
                 , result          :: Maybe GameResult
                 } deriving Eq

toPGN :: Game -> PGN
-- Pre : Blancas y Negras han sido asignadas
toPGN (Game chan time (Just w) (Just b) startPos _ _ history res) = 
    P.PGN "Juego casual" chan (showGregorian $ utctDay time) "-" w b res startPos (map unifyTurnMoves $ reverse history)
    where
        unifyTurnMoves (n, "..", Just blackMove)      = show n ++ "." ++ ".. "++ blackMove
        unifyTurnMoves (n, whiteMove, Nothing)        = show n ++ "." ++ " "  ++ whiteMove
        unifyTurnMoves (n, whiteMove, Just blackMove) = show n ++ "." ++ " "  ++ whiteMove ++ " " ++ blackMove

showPGN :: PGN -> String
showPGN (P.PGN event site date rnd whi blk res stpos mvs) = 
    tagPGN "Event" event ++
    tagPGN "Site"  site  ++
    tagPGN "Date"  date  ++
    tagPGN "Round" rnd   ++
    tagPGN "White" whi   ++
    tagPGN "Black" blk   ++
    tagPGN "Result" (displayResult res) ++
    (if stpos == Nothing then [] else tagPGN "FEN" (toFEN $ fromJust stpos)) ++
    "\n" ++
    displayMoves 1 mvs ++
    endString res
    where
        tagPGN tag val = "[" ++ tag ++ " " ++ "\"" ++ val ++ "\"" ++ "]" ++ "\n"
        displayMoves _ [] = []       
        displayMoves n (m:ms) = (m ++ if n `mod` 8 == 0 then "\n" else " ") ++ displayMoves (n+1) ms
        displayResult Nothing  = "*"
        displayResult (Just r) = show r
        endString Nothing  = "\n" ++ "*"
        endString (Just r) = show r

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

savePGN :: String -> PGN -> IO ()
savePGN filename pgn = do
    outh <- openFile filename WriteMode
    hPutStrLn outh (showPGN pgn)
    hClose outh

