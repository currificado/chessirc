import Chess
import Chess.FEN

import Game
import Input

import Data.Char (isSpace)
import Data.List (intercalate)
import Control.Monad (when)
import Data.Time (getCurrentTime)
import Network.Socket
import System.Environment (getArgs)
import System.IO (BufferMode(..), Handle, hClose, hIsEOF, hSetBuffering, hSetEncoding, hGetContents, hGetLine, hPutStrLn, IOMode(..), utf8)
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
                        Right (Move       nick move) -> hPutStrLn handle (show (Move nick move))
                        Right (Input.Draw nick     ) -> hPutStrLn handle (show (Input.Draw nick))
                        Right (Resign     nick     ) -> hPutStrLn handle (show (Resign nick))
        True  -> do handleCLOSE handle addr
    where
        chomp       = reverse . dropWhile isSpace . reverse -- strip trailing spaces (including newlines)
        showFlatten = intercalate " " . lines . show

toLog :: SockAddr -> String -> IO ()
toLog addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

delimiter :: String
delimiter = "<:=:>"

handleSESSION :: Channel -> Maybe Game -> Handle -> IO (Maybe Game)
handleSESSION chan game handle =
    case game of
         Nothing -> do hPutStrLn handle ("Se ha iniciado una sesión en " ++ chan ++ ".")
                       return (Just (Game chan getCurrentTime Nothing Nothing defaultBoard [] Nothing))
         Just g  -> do hPutStrLn handle ("Ya hay una sesión activa en " ++ site g ++ ".")
                       return (Just g)

handleCLOSE :: Handle -> SockAddr -> IO ()
handleCLOSE handle addr = do
    hClose handle
    toLog addr "Client disconnected"

handleREGISTER :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleREGISTER nick game handle = do
    case game of
         Nothing                                   -> do hPutStrLn handle "Primero debe iniciar una sesión."
                                                         return game
         Just g@(Game _ _ Nothing  Nothing  _ _ _) -> do hPutStrLn handle ("Se ha registrado '" ++ nick ++ "' para jugar con Blancas.")
                                                         return (Just (g { white = Just nick }))
         Just g@(Game _ _ (Just _) Nothing  _ _ _) -> do hPutStrLn handle ("Se ha registrado '" ++ nick ++ "' para jugar con Negras.")
                                                         return (Just (g { black = Just nick }))
         Just g@(Game _ _ (Just _) (Just _) _ _ _) -> do hPutStrLn handle ("Ambos jugadores ya han sido registrados.")
                                                         return game

handleSTART :: Nick -> Maybe Game -> Handle -> IO (Maybe Game)
handleSTART _ Nothing handle = do
    hPutStrLn handle "Primero debe iniciar una sesión."
    return Nothing
handleSTART _ (Just g@(Game _ _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Blancas.")
    return (Just g)
handleSTART _ (Just g@(Game _ _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Negras.")
    return (Just g)
handleSTART nick (Just g@(Game _ _ (Just player1) (Just player2) _ _ _)) handle = do
    if nick == player1 || nick == player2 then
        do hPutStrLn handle ("Blancas mueven. Es el turno de '" ++ player1 ++ "'.")
           hPutStrLn handle delimiter 
           hPutStrLn handle (stringifyBoard White (Game.board g))
    else
        hPutStrLn handle ("'" ++ nick ++ "' no está habilitado para inciar la partida.")
    return (Just g)

handleMOVE :: Nick -> SANMove -> Maybe Game -> Handle -> IO (Maybe Game)
handleMOVE _ _ Nothing handle = do
    hPutStrLn handle "Primero debe iniciar una sesión."
    return Nothing
handleMOVE _ _ (Just g@(Game _ _ Nothing _ _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Blancas.")
    return (Just g)
handleMOVE _ _ (Just g@(Game _ _ (Just _) Nothing _ _ _)) handle = do
    hPutStrLn handle ("No hay un jugador asignado para las Negras.")
    return (Just g)
handleMOVE nick move (Just g@(Game _ _ (Just player1) (Just player2) brd hs _)) handle = do
    if nick == player1 || nick == player2 then
        do hPutStrLn handle ("Blancas mueven. Es el turno de '" ++ player1 ++ "'.")
           hPutStrLn handle delimiter 
           hPutStrLn handle (stringifyBoard White (Game.board g))
    else
        hPutStrLn handle ("'" ++ nick ++ "' no está habilitado para inciar la partida.")
    return (Just g)
