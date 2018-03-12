{-

Módulo Main con el sevidor de ajedrez

-}

import Chess
import Chess.FEN

import Game as G
import Input

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Monad (when)
import Data.Time (getCurrentTime, utctDay)
import Network.Socket
import System.Environment (getArgs)
import System.IO (BufferMode(..), Handle, 
                  hClose, hIsEOF, hSetBuffering, 
                  hSetEncoding, hGetContents, 
                  hGetLine, hPutStr, hPutStrLn, 
                  IOMode(..), utf8)
import Control.Concurrent (forkIO)

-- | Puerto del servidor
port = "4321"

-- | Mensajes salientes
data OutgMsg = StartSession Channel
             | ExSession Channel
             | NonExSession
             | RegWhite Nick
             | RegBlack Nick
             | AlrdyReg
             | AlrdyStarted
             | MissingWhite
             | MissingBlack
             | UnstarteredGame
             | WhtMoves Nick
             | BlkMoves Nick
             | WrongTurn Nick
             | WhtOffersDraw Nick
             | BlkOffersDraw Nick
             | IllegalMovement
             | UnableStart
             | UnableMove
             | UnableOfferDraw
             | UnableAcceptDraw
             | UnableResign
            deriving Eq

instance Show OutgMsg where
    show (StartSession chan)   = "Se ha iniciado una sesión en " ++ chan ++ "."
    show (ExSession chan)      = "Ya hay una sesión activa en " ++ chan ++ "."
    show  NonExSession         = "Primero debe iniciar una sesión."
    show (RegWhite     nick)   = "Se ha registrado '" ++ nick ++ "' para jugar con Blancas."
    show (RegBlack     nick)   = "Se ha registrado '" ++ nick ++ "' para jugar con Negras."
    show  AlrdyReg             = "Ambos jugadores ya han sido registrados."
    show  AlrdyStarted         = "Ya hay un juego en curso."
    show  MissingWhite         = "No hay un jugador asignado para las Blancas."
    show  MissingBlack         = "No hay un jugador asignado para las Negras."
    show  UnstarteredGame      = "Aún no se ha iniciado la partida."
    show (WhtMoves       nick) = "Blancas mueven. Es el turno de '" ++ nick ++ "'."
    show (BlkMoves       nick) = "Negras mueven. Es el turno de '" ++ nick ++ "'."
    show (Main.WrongTurn nick) = "No es el turno de '" ++ nick ++ "'."
    show (WhtOffersDraw  nick) = "Blancas ('" ++ nick ++ "') ofrecen tablas."
    show (BlkOffersDraw  nick) = "Negras ('" ++ nick ++ "') ofrecen tablas."
    show  IllegalMovement      = "Movimiento ilegal. Intente de nuevo."
    show  UnableStart          = "Usted no participa del juego, por tanto no puede iniciar la partida."
    show  UnableMove           = "Usted no participa del juego, por tanto no está habilitado para mover."
    show  UnableOfferDraw      = "No le corresponde ofrecer tablas."
    show  UnableAcceptDraw     = "No le corresponde aceptar tablas."
    show  UnableResign         = "Usted no participa del juego, por tanto no puede retirarse en lugar de uno de los jugadores."

delimiter :: String
delimiter = "<:=:>"

endmark :: String
endmark = "-###-"

toLog :: SockAddr -> String -> IO ()
toLog addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

opposite :: Color -> Color
opposite White = Black
opposite Black = White

runServer :: String -> IO ()
runServer port = withSocketsDo $ do 
    addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serverAddr = head addrInfos
    serverSock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind serverSock (addrAddress serverAddr)
    listen serverSock 5
    putStrLn $ "Listening on " ++ show port ++ "..."
    procRequests serverSock

procRequests :: Socket -> IO ()
procRequests serverSock = do
    (clientSock, clientAddr) <- accept serverSock
    handle <- socketToHandle clientSock ReadWriteMode
    hSetBuffering handle LineBuffering
    hSetEncoding handle utf8
    toLog clientAddr "Client connnected"
    forkIO $ gameSession Nothing handle clientAddr
    procRequests serverSock

gameSession :: Maybe Game -> Handle -> SockAddr -> IO ()
gameSession game handle addr = do
    isEOF <- hIsEOF handle
    case isEOF of
        False -> do line <- hGetLine handle
                    let m = parseMessage (chomp line)
                    case m of
                        Left  err                    -> do hPutStrLn handle (showFlatten err)
                                                           hPutStrLn handle endmark
                                                           gameSession game handle addr
                        Right (Session    chan     ) -> do g <- handleSESSION chan game handle
                                                           hPutStrLn handle endmark 
                                                           gameSession g handle addr
                        Right (Close               ) -> do handleCLOSE handle addr
                        Right (Register   nick     ) -> do g <- handleREGISTER nick game handle
                                                           hPutStrLn handle endmark
                                                           gameSession g handle addr
                        Right (Start      nick     ) -> do g <- handleSTART nick game handle
                                                           hPutStrLn handle endmark
                                                           gameSession g handle addr
                        Right (Move       nick move) -> do g <- handleMOVE nick move game handle
                                                           checkAfterMOVE game g
                        Right (Input.Draw nick     ) -> do g <- handleDRAW nick game handle
                                                           checkAfterDRAWorRESIGN game g
                        Right (Resign     nick     ) -> do g <- handleRESIGN nick game handle
                                                           checkAfterDRAWorRESIGN game g

        True  -> handleCLOSE handle addr
    where
        chomp       = reverse . dropWhile isSpace . reverse -- strip trailing spaces (including newlines)
        showFlatten = intercalate " " . lines . show
        showRecentHistory (n, m1, Nothing) = (show n) ++ ". " ++ m1
        showRecentHistory (n, m1, Just m2) = (show n) ++ ". " ++ m1 ++ " " ++ m2
        showWhoMoves g = let player1 = fromJust $ white g
                             player2 = fromJust $ black g
                             color   = turn (G.board g) in
                             (if color == White then
                                show $ WhtMoves player1
                              else
                                show $ BlkMoves player2)
        checkAfterMOVE previous next =
            if previous == next then -- game state didn't change
                do hPutStrLn handle endmark
                   gameSession previous handle addr
            else
                do let g = fromJust next
                   hPutStrLn handle $ showRecentHistory (head $ history g)
                   case (result g) of
                       Nothing -> do hPutStrLn handle (showWhoMoves g)
                                     hPutStrLn handle delimiter
                                     let brd = G.board g
                                     hPutStr handle (stringifyBoard (turn brd) brd)
                                     hPutStrLn handle endmark
                                     gameSession next handle addr
                       Just res-> do hPutStrLn handle (show res)
                                     hPutStrLn handle delimiter
                                     let brd = G.board g
                                     hPutStr handle (stringifyBoard (turn brd) brd)
                                     hPutStrLn handle endmark
                                     handleCLOSE handle addr
        checkAfterDRAWorRESIGN previous next = do
            if next == Nothing then
                do hPutStrLn handle endmark
                   gameSession previous handle addr
            else
                do let g = fromJust next
                   case (result g) of
                        Nothing -> do hPutStrLn handle endmark
                                      gameSession next handle addr
                        Just res-> do hPutStrLn handle (show res)
                                      hPutStrLn handle endmark
                                      handleCLOSE handle addr

-- | Handler de SESSION
handleSESSION :: Channel -> Maybe Game -> Handle -> IO (Maybe Game)
handleSESSION chan game handle =
    case game of
        Nothing -> do hPutStrLn handle (show (StartSession chan))
                      t <- getCurrentTime 
                      return (Just (Game chan (utctDay t) False False Nothing Nothing defaultBoard [] Nothing))
        Just g  -> do hPutStrLn handle (show $ ExSession (site g))
                      return (Just g)

-- | Handler de CLOSE
handleCLOSE :: Handle -> SockAddr -> IO ()
handleCLOSE handle addr = do
    hClose handle
    toLog addr "Client disconnected"

-- | Handler de REGISTER
handleREGISTER :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleREGISTER nick game handle = do
    case game of
        Nothing                                       -> do hPutStrLn handle (show NonExSession)
                                                            return game
        Just g@(Game _ _ _ _ Nothing  Nothing  _ _ _) -> do hPutStrLn handle (show $ RegWhite nick)
                                                            return (Just (g { white = Just nick }))
        Just g@(Game _ _ _ _ (Just _) Nothing  _ _ _) -> do hPutStrLn handle (show $ RegBlack nick)
                                                            return (Just (g { black = Just nick }))
        Just g@(Game _ _ _ _ (Just _) (Just _) _ _ _) -> do hPutStrLn handle (show AlrdyReg)
                                                            return game
-- | Handler de START
handleSTART :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleSTART _ Nothing handle = do
    hPutStrLn handle (show NonExSession)
    return Nothing
handleSTART _ (Just g@(Game _ _ False _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle (show MissingWhite)
    return (Just g)
handleSTART _ (Just g@(Game _ _ False _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle (show MissingBlack)
    return (Just g)
handleSTART nick (Just g@(Game _ _ False _ (Just player1) (Just player2) _ _ _)) handle = do
    if nick == player1 || nick == player2 then
        do hPutStrLn handle (show $ WhtMoves player1)
           hPutStrLn handle delimiter 
           hPutStr   handle (stringifyBoard White (G.board g))
           return (Just g {active = True})
    else
        do hPutStrLn handle (show UnableStart)
           return (Just g)
handleSTART nick (Just g@(Game _ _ True _ _ _ _ _ _)) handle = do
    hPutStrLn handle (show AlrdyStarted)
    return (Just g)

-- | Handler de MOVE
handleMOVE :: Nick -> SANMove -> Maybe Game -> Handle -> IO (Maybe Game)
handleMOVE _ _ Nothing handle = do
    hPutStrLn handle (show NonExSession)
    return Nothing
handleMOVE _ _ (Just g@(Game _ _ False _ _ _ _ _ _)) handle = do
    hPutStrLn handle (show UnstarteredGame)
    return (Just g)
handleMOVE nick move (Just g@(Game _ _ True _ (Just player1) (Just player2) brd hs _)) handle = do
    if nick == player1 || nick == player2 then
        if (nick == player1 && turn brd == White) || (nick == player2 && turn brd == Black) then
            do let mbrd = moveSAN move brd
               case mbrd of
                   Left  _    -> do hPutStrLn handle (show IllegalMovement)
                                    return (Just g)
                   Right brd' -> do let h = history g
                                    let c = turn brd'
                                    if mate c brd' then
                                        return (Just g { G.board   = brd',
                                                         history   = addMove (move++"#") h,
                                                         result    = Just (won (opposite c)),
                                                         drawoffer = False })
                                    else
                                        {-if stalemate c brd' then
                                            return (Just (g { G.board = brd', history = addMove move h, result = Just G.Draw }))
                                        else-}
                                            let move' = if check c brd' then move++"+" else move in
                                            return (Just (g { G.board   = brd',
                                                              history   = addMove move' h,
                                                              drawoffer = False }))
        else
             do hPutStrLn handle (show $ Main.WrongTurn nick)
                return (Just g)
    else
        do hPutStrLn handle (show UnableMove)
           return (Just g)
    where
        won c = if c == White then WhiteWon else BlackWon
        addMove m1 []                   = [(1, m1, Nothing)]
        addMove m2 ((n,m1,Nothing):hs)  = (n, m1, Just m2):hs
        addMove m1 h@((n,_,_):hs)       = (n+1, m1, Nothing):h

-- | Handler de DRAW
handleDRAW :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleDRAW _ Nothing handle = do
    hPutStrLn handle (show NonExSession)
    return Nothing
handleDRAW _ (Just g@(Game _ _ False _ _ _ _ _ _)) handle = do
    hPutStrLn handle (show UnstarteredGame)
    return (Just g)
handleDRAW nick (Just g@(Game _ _ True True (Just player1) (Just player2) brd _ _)) handle =
    if (turn brd == White && nick == player2) || (turn brd == Black && nick == player1) then
        return (Just (g { result = Just G.Draw }))
    else
        do hPutStrLn handle (show UnableAcceptDraw)
           return (Just g)
handleDRAW nick (Just g@(Game _ _ True False (Just player1) (Just player2) brd _ _)) handle =
    if (turn brd == White && nick == player1) || (turn brd == Black && nick == player2) then
        do hPutStrLn handle (offerDrawMsg (turn brd) nick)
           return (Just (g { drawoffer = True }))
    else
        do hPutStrLn handle (show UnableOfferDraw)
           return (Just g)
    where
        offerDrawMsg color nick = if color == White then 
                                      show $ WhtOffersDraw nick
                                  else
                                      show $ BlkOffersDraw nick
                                      
-- | Handler de RESIGN
handleRESIGN :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleRESIGN _ Nothing handle = do
    hPutStrLn handle (show NonExSession)
    return Nothing
handleRESIGN _ (Just g@(Game _ _ False _ _ _ _ _ _)) handle = do
    hPutStrLn handle (show UnstarteredGame)
    return (Just g)
handleRESIGN nick (Just g@(Game _ _ True _ (Just player1) (Just player2) brd _ _)) handle =
    if nick == player1 || nick == player2 then
        return (Just (g { drawoffer = False, result = Just $ won (winner (turn brd)) }))
    else
        do hPutStrLn handle (show UnableResign)
           return (Just g)
    where
        winner c = if nick == player1 then (if nick == player2 then opposite c else Black) else White
        won c    = if c == White then WhiteWon else BlackWon

-- | Función main
main = runServer port
