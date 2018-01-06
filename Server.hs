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
import System.IO (BufferMode(..), Handle, hClose, hIsEOF, hSetBuffering, hSetEncoding, hGetContents, hGetLine, hPutStr, hPutStrLn, IOMode(..), utf8)
import Control.Concurrent (forkIO)

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
                                                           gameSession game handle addr
                        Right (Session    chan     ) -> do g <- handleSESSION chan game handle
                                                           gameSession g handle addr
                        Right (Close               ) -> do handleCLOSE handle addr
                        Right (Register   nick     ) -> do g <- handleREGISTER nick game handle
                                                           gameSession g handle addr
                        Right (Start      nick     ) -> do g <- handleSTART nick game handle
                                                           gameSession g handle addr
                        Right (Move       nick move) -> do g <- handleMOVE nick move game handle
                                                           if g == game then -- game state didn't change
                                                               gameSession game handle addr
                                                           else
                                                               do let g' = fromJust g
                                                                  hPutStrLn handle $ showRecentHistory (head $ history g')
                                                                  case (result g') of
                                                                      Nothing -> do hPutStrLn handle (showWhoMoves g')
                                                                                    hPutStrLn handle delimiter
                                                                                    let brd = G.board g'
                                                                                    hPutStr handle (stringifyBoard (turn brd) brd)
                                                                                    gameSession g handle addr
                                                                      Just res-> do hPutStrLn handle (show res)
                                                                                    hPutStrLn handle delimiter
                                                                                    let brd = G.board g'
                                                                                    hPutStr handle (stringifyBoard (turn brd) brd)
                                                                                    handleCLOSE handle addr
                        Right (Input.Draw nick     ) -> do g <- handleDRAW nick game handle
                                                           if g == Nothing then
                                                               gameSession game handle addr
                                                           else
                                                               do let g' = fromJust g
                                                                  case (result g') of
                                                                      Nothing -> gameSession g handle addr
                                                                      Just res-> do hPutStrLn handle (show res)
                                                                                    handleCLOSE handle addr
                        Right (Resign     nick     ) -> do g <- handleRESIGN nick game handle
                                                           if g == Nothing then
                                                               gameSession game handle addr
                                                           else
                                                               do let g' = fromJust g
                                                                  case (result g') of
                                                                      Nothing -> gameSession g handle addr
                                                                      Just res-> do hPutStrLn handle (show res)
                                                                                    handleCLOSE handle addr
        True  -> do handleCLOSE handle addr
    where
        chomp       = reverse . dropWhile isSpace . reverse -- strip trailing spaces (including newlines)
        showFlatten = intercalate " " . lines . show
        showRecentHistory (n, m1, Nothing) = (show n) ++ ". " ++ m1
        showRecentHistory (n, m1, Just m2) = (show n) ++ ". " ++ m1 ++ " " ++ m2
        showWhoMoves g = let player1 = fromJust $ white g
                             player2 = fromJust $ black g
                             color   = turn (G.board g) in (if color == White then ("Blancas mueven. Es el turno de '" ++ player1 ++ "'.") else ("Negras mueven. Es el turno de '" ++ player2 ++ "'."))

toLog :: SockAddr -> String -> IO ()
toLog addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

delimiter :: String
delimiter = "<:=:>"

handleSESSION :: Channel -> Maybe Game -> Handle -> IO (Maybe Game)
handleSESSION chan game handle =
    case game of
         Nothing -> do hPutStrLn handle ("Se ha iniciado una sesión en " ++ chan ++ ".")
                       t <- getCurrentTime 
                       return (Just (Game chan (utctDay t) False Nothing Nothing defaultBoard [] Nothing))
         Just g  -> do hPutStrLn handle ("Ya hay una sesión activa en " ++ site g ++ ".")
                       return (Just g)

handleCLOSE :: Handle -> SockAddr -> IO ()
handleCLOSE handle addr = do
    hClose handle
    toLog addr "Client disconnected"

handleREGISTER :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleREGISTER nick game handle = do
    case game of
         Nothing                                     -> do hPutStrLn handle "Primero debe iniciar una sesión."
                                                           return game
         Just g@(Game _ _ _ Nothing  Nothing  _ _ _) -> do hPutStrLn handle ("Se ha registrado '" ++ nick ++ "' para jugar con Blancas.")
                                                           return (Just (g { white = Just nick }))
         Just g@(Game _ _ _ (Just _) Nothing  _ _ _) -> do hPutStrLn handle ("Se ha registrado '" ++ nick ++ "' para jugar con Negras.")
                                                           return (Just (g { black = Just nick }))
         Just g@(Game _ _ _ (Just _) (Just _) _ _ _) -> do hPutStrLn handle ("Ambos jugadores ya han sido registrados.")
                                                           return game

handleSTART :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleSTART _ Nothing handle = do
    hPutStrLn handle "Primero debe iniciar una sesión."
    return Nothing
handleSTART _ (Just g@(Game _ _ _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Blancas.")
    return (Just g)
handleSTART _ (Just g@(Game _ _ _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Negras.")
    return (Just g)
handleSTART nick (Just g@(Game _ _ _ (Just player1) (Just player2) _ _ _)) handle = do
    if nick == player1 || nick == player2 then
        do hPutStrLn handle ("Blancas mueven. Es el turno de '" ++ player1 ++ "'.")
           hPutStrLn handle delimiter 
           hPutStr   handle (stringifyBoard White (G.board g))
    else
        hPutStrLn handle ("'" ++ nick ++ "' no está habilitado para inciar la partida.")
    return (Just g)

handleMOVE :: Nick -> SANMove -> Maybe Game -> Handle -> IO (Maybe Game)
handleMOVE _ _ Nothing handle = do
    hPutStrLn handle "Primero debe iniciar una sesión."
    return Nothing
handleMOVE _ _ (Just g@(Game _ _ _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Blancas.")
    return (Just g)
handleMOVE _ _ (Just g@(Game _ _ _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Negras.")
    return (Just g)
handleMOVE nick move (Just g@(Game _ _ _ (Just player1) (Just player2) brd hs _)) handle = do
    if nick == player1 || nick == player2 then
        if (nick == player1 && turn brd == White) || (nick == player2 && turn brd == Black) then
            do let mbrd = moveSAN move brd
               case mbrd of
                    Left  err  -> do hPutStrLn handle $ showMoveError err ++ " Intente de nuevo."
                                     return (Just g)
                    Right brd' -> do let h = history g
                                     let c = turn brd'
                                     if mate c brd' then
                                         return (Just g { G.board   = brd',
                                                          history   = addMove (move++"#") h,
                                                          result    = Just (won (opposite c)),
                                                          drawOffer = False })
                                     else
                                         {-if stalemate c brd' then
                                             return (Just (g { G.board = brd', history = addMove move h, result = Just G.Draw }))
                                         else-}
                                             let move' = if check c brd' then move++"+" else move in
                                             return (Just (g { G.board   = brd',
                                                               history   = addMove move' h,
                                                               drawOffer = False }))
        else
             do hPutStrLn handle ("No es el turno de '" ++ nick ++ "'.")
                return (Just g)
    else
        do hPutStrLn handle ("'" ++ nick ++ "' no está habilitado para mover.")
           return (Just g)
    where
        showMoveError WrongTurn   = "Ha querido mover una pieza de su rival."
        showMoveError NoPiece     = "No hay ninguna pieza en el escaque de origen."
        showMoveError IsCheck     = "No olvide que se encuentra en jaque."
        showMoveError CausesCheck = "Su movimiento deja indefenso al rey."
        showMoveError InvalidMove = "La pieza seleccionada no puede hacer ese movimiento."
        showMoveError OverPiece   = "Hay piezas que se interponen en el camino."
        showMoveError CapturesOwn = "Este movimiento captura una de sus propias piezas."
        showMoveError NoParse     = "Movimiento ilegal."
        opposite White = Black
        opposite Black = White
        won c = if c == White then WhiteWon else BlackWon
        addMove m1 []                   = [(1, m1, Nothing)]
        addMove m2 ((n,m1,Nothing):hs)  = (n, m1, Just m2):hs
        addMove m1 h@((n,_,_):hs)       = (n+1, m1, Nothing):h

handleDRAW :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleDRAW _ Nothing handle = do
    hPutStrLn handle "Primero debe iniciar una sesión."
    return Nothing
handleDRAW _ (Just g@(Game _ _ _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Blancas.")
    return (Just g)
handleDRAW _ (Just g@(Game _ _ _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Negras.")
    return (Just g)
handleDRAW nick (Just g@(Game _ _ True (Just player1) (Just player2) brd _ _)) handle =
    if (turn brd == White && nick == player2) || (turn brd == Black && nick == player1) then
        return (Just (g { result = Just G.Draw }))
    else
        do hPutStrLn handle "No le corresponde aceptar tablas."
           return (Just g)
handleDRAW nick (Just g@(Game _ _ False (Just player1) (Just player2) brd _ _)) handle =
    if (turn brd == White && nick == player1) || (turn brd == Black && nick == player2) then
        do hPutStrLn handle (offerDrawMsg (turn brd) nick)
           return (Just (g { drawOffer = True }))
    else
        do hPutStrLn handle "No le corresponde ofrecer tablas."
           return (Just g)
    where
        offerDrawMsg color nick = if color == White then "Blancas ('" ++ nick ++ "') ofrecen tablas." else "Negras ('" ++ nick ++ "') ofrecen tablas."

handleRESIGN :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleRESIGN _ Nothing handle = do
    hPutStrLn handle "Primero debe iniciar una sesión."
    return Nothing
handleRESIGN _ (Just g@(Game _ _ _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Blancas.")
    return (Just g)
handleRESIGN _ (Just g@(Game _ _ _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Negras.")
    return (Just g)
handleRESIGN nick (Just g@(Game _ _ _ (Just player1) (Just player2) brd _ _)) handle =
    if nick == player1 || nick == player2 then
        return (Just (g { drawOffer = False, result = Just $ won (winner) }))
    else
        do hPutStrLn handle "Usted no participa del juego."
           return (Just g)
    where
        winner = if nick == player1 then Black else White
        won c  = if c == White then WhiteWon else BlackWon

main = runServer "4321"
