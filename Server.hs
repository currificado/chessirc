import Game
import Input

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
                        Left  err                     -> hPutStrLn handle (show err)
                        Right (Session    chan      ) -> hPutStrLn handle (show (Session chan))
                        Right (Close                ) -> do hPutStrLn handle (show Close)
                                                            hClose handle
                                                            toLog addr "Client disconnected"
                        Right (Register   nick      ) -> hPutStrLn handle (show (Register nick))
                        Right (Start      nick      ) -> hPutStrLn handle (show (Start nick))
                        Right (Move       nick move ) -> hPutStrLn handle (show (Move nick move))
                        Right (Input.Draw nick      ) -> hPutStrLn handle (show (Input.Draw nick))
                        Right (Resign     nick      ) -> hPutStrLn handle (show (Resign nick))
                    loop m
        True  -> do hClose handle
                    toLog addr "Client disconnected"
    where
        chomp = reverse . dropWhile isSpace . reverse -- strip trailing spaces (including newlines)
        loop m = when (m /= Right Close) (gameSession game handle addr)

toLog :: SockAddr -> String -> IO ()
toLog addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg
