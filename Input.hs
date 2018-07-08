module Input ( Channel
             , FEN
             , Message(..)
             , Nick
             , SANMove
             , parseMessage
             ) where

{-

Parser para los mensajes entrantes

-}

import Chess (Color(..))

import Data.Char (isSpace, isLatin1)

import Text.Parsec ((<|>), ParseError, eof, many1, parse, satisfy, try)
import Text.Parsec.Char (char, space, string)
import Text.Parsec.String (Parser)

import Control.Applicative ((<*))

type Channel = String
type Nick    = String
type SANMove = String
type FEN     = String

data Message = Session Channel
             | Close
             | Register Nick
             | Start    Nick FEN
             | Move     Nick SANMove
             | Board Color
             | Position
             | PGN
             | Draw     Nick
             | Resign   Nick
             deriving (Eq, Show)

-- | Helpers
word :: Parser String
word = many1 (satisfy (\x -> not (isSpace x)))

-- | Helpers
fen :: Parser String
fen = many1 (satisfy isLatin1)


-- | Parsers para los distintos mensajes
session :: Parser Message
session = do string "SESSION"
             space
             hashtag <- char '#'
             chan    <- word
             return (Session (hashtag:chan))

close :: Parser Message
close = do string "CLOSE"
           return Close

register :: Parser Message
register = do string "REGISTER"
              space
              nick <- word
              return (Register nick)

start :: Parser Message
start = do string "START"
           space
           nick <- word
           space
           fen  <- fen
           return (Start nick fen)

move :: Parser Message
move = do string "MOVE"
          space
          nick <- word
          space
          move <- word
          return (Move nick move)

board :: Parser Message
board = do string "BOARD"
           space
           color <- string "White" <|> string "Black"
           return (Board (cl color))
    where
        cl "White" = White
        cl "Black" = Black

position :: Parser Message
position = do string "POSITION"
              return Position

pgn :: Parser Message
pgn = do string "PGN"
         return PGN

draw :: Parser Message
draw = do string "DRAW"
          space
          nick <- word
          return (Draw nick)
             
resign :: Parser Message
resign = do string "RESIGN"
            space
            nick <- word
            return (Resign nick)

            
-- | Parser principal
parserMsg :: Parser Message
parserMsg = try session
        <|> close
        <|> try register
        <|> start
        <|> move
        <|> board
        <|> try position
        <|> pgn
        <|> draw
        <|> resign

-- | Función que realiza el parsing
parseMessage :: String -> Either ParseError Message
parseMessage = parse (parserMsg <* eof) ""
