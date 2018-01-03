import Chess
import Chess.FEN

import Game
import Input

import Data.List (intercalate)
import Control.Monad (when)
import Data.Char (isSpace)
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
                        Right (Close               ) -> handleCLOSE handle addr
                        Right (Register   nick     ) -> do g <- handleREGISTER nick game handle
                                                           gameSession g handle addr
                        Right (Start      nick     ) -> hPutStrLn handle (show (Start nick))
                        Right (Move       nick move) -> hPutStrLn handle (show (Move nick move))
                        Right (Input.Draw nick     ) -> hPutStrLn handle (show (Input.Draw nick))
                        Right (Resign     nick     ) -> hPutStrLn handle (show (Resign nick))
        True  -> handleCLOSE handle addr
    where
        chomp       = reverse . dropWhile isSpace . reverse -- strip trailing spaces (including newlines)
        showFlatten = intercalate " " . lines . show

toLog :: SockAddr -> String -> IO ()
toLog addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

handleSESSION :: Channel -> Maybe Game -> Handle -> IO (Maybe Game)
handleSESSION chan game handle =
    case game of
         Nothing -> do hPutStrLn handle ("Se ha iniciado una sesión en " ++ chan ++ ".")
                       return (Just (Game chan "" Nothing Nothing defaultBoard [] Nothing))
         Just g  -> do hPutStrLn handle ("Ya hay una sesión activa en " ++ site g ++ ".")
                       return (Just g)

handleCLOSE :: Handle -> SockAddr -> IO ()
handleCLOSE handle addr = do hClose handle
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
