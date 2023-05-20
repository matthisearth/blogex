-- blogex
-- OutsideInteract.hs

module OutsideInteract (
    RwMvar,
    withServer,
    getResult ) where

import qualified System.Process as P
import qualified Control.Concurrent as C
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import qualified System.IO as IO
import Control.Monad (forM)
import Text.Read (readMaybe)

data RwMvar = RwMvar {
    wMvar :: C.MVar [String],
    rMvar :: C.MVar String }

devNullWrite :: IO IO.Handle
devNullWrite = IO.openFile "/dev/null" IO.WriteMode

portNum :: String
portNum = "7935" -- hardcoded port

bufSize :: Int
bufSize = 1024

withServer :: FilePath -> (RwMvar -> IO a) -> IO a
withServer jsFile f = do
    -- Start server and nodejs
    let hints = S.defaultHints {
        S.addrFlags = [S.AI_PASSIVE],
        S.addrSocketType = S.Stream }
    serveraddr <- head <$> S.getAddrInfo (Just hints) Nothing (Just portNum)
    sock <- S.socket (S.addrFamily serveraddr) S.Stream S.defaultProtocol
    S.setSocketOption sock S.ReuseAddr 1 -- reuse address right away
    S.setSocketOption sock S.ReusePort 1 -- reuse port right away
    S.bind sock (S.addrAddress serveraddr)
    S.listen sock 1 -- only one connection needed
    outRwMvar <- C.newEmptyMVar
    inRwMVar <- C.newEmptyMVar
    nodeMvar <- C.newEmptyMVar
    let rwMvar = RwMvar { wMvar = outRwMvar, rMvar = inRwMVar }
    _ <- C.forkIO $ do
        (rwSock, _) <- S.accept sock
        handleServer rwMvar rwSock
        S.gracefulClose rwSock 1000
    _ <- C.forkIO $ do
        outNull <- P.UseHandle <$> devNullWrite
        errNull <- P.UseHandle <$> devNullWrite
        _ <- P.withCreateProcess
            (P.proc "node" [jsFile, portNum])
                { P.std_out = outNull, P.std_err = errNull }
            (\_ _ _ h -> P.waitForProcess h)
        C.putMVar nodeMvar ()
    -- Execute actions
    res <- f rwMvar
    -- Make client close the connection and hence terminate
    _ <- getResult rwMvar ["stop"]
    _ <- C.takeMVar nodeMvar
    _ <- S.gracefulClose sock 1000
    -- Return result
    return res

handleServer :: RwMvar -> S.Socket -> IO ()
handleServer rwMvar rwSock = do
    o <- C.takeMVar $ wMvar rwMvar
    if o == ["stop"]
    then C.putMVar (rMvar rwMvar) "r:ok"
    else do
        is <- forM o $ \x -> (sendString rwSock x) >> (receiveString rwSock)
        _ <- C.putMVar (rMvar rwMvar) (last is)
        handleServer rwMvar rwSock

getResult :: RwMvar -> [String] -> IO (Either String String)
getResult rwMvar inputString = do
    _ <- C.putMVar (wMvar rwMvar) inputString
    value <- C.takeMVar (rMvar rwMvar)
    case value of
        ('r':':':xs) -> return $ Right xs
        ('e':':':xs) -> return $ Left xs
        _ -> error "Error: Invalid server response."

sendString :: S.Socket -> String -> IO ()
sendString rwSock s = do
    let
        bs = BU.fromString s
        n = BU.length bs
        send = B.concat [BU.fromString (show n ++ "@"), bs]
    _ <- SB.sendAll rwSock send
    return ()

receiveString :: S.Socket -> IO String
receiveString rwSock = stripHead <$> finalBuf
    where
        finalBuf = recIncr (BU.fromString "") 0 Nothing
        recIncr :: B.ByteString -> Int -> (Maybe Int) -> IO B.ByteString
        recIncr buf bufLen resultTot = do
            nextBuf <- SB.recv rwSock bufSize
            let
                newBuf = B.concat [buf, nextBuf]
                newBufLen = bufLen + (B.length nextBuf)
                newResultTot = case resultTot of
                    Just n -> Just n
                    Nothing -> getNum newBuf
            case newResultTot of
                Just n -> if newBufLen == n
                    then return newBuf
                    else recIncr newBuf newBufLen (Just n)
                Nothing -> recIncr newBuf newBufLen Nothing

getNum :: B.ByteString -> Maybe Int
getNum bs =
    do
        nStr <- getHead s
        n <- readMaybe nStr
        let extraLen = B.length $ BU.fromString $ nStr ++ "@"
        return $ n + extraLen
    where
        s = BU.toString bs
        getHead ('@':_) = Just ""
        getHead (x:xs) = (x:) <$> getHead xs
        getHead _ = Nothing

stripHead :: B.ByteString -> String
stripHead bs = stripHeadString $ BU.toString bs
    where
        stripHeadString ('@':xs) = xs
        stripHeadString (_:xs) = stripHeadString xs
        stripHeadString _ = error "Error: Server sent no message header."
