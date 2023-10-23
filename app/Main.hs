{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BBC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Network.Simple.TCP

import Debug.Trace (trace)
import HttpRequest (HttpRequest (..), parseHttpHeader)

recvAll :: (MonadIO m) => Socket -> m BB.ByteString
recvAll sock =
    let
        block = 1024
        inner acc = do
            dataRecv <- recv sock block
            case dataRecv of
                Nothing -> trace "faa" $ pure acc
                Just d ->
                    trace "foo"
                        $ if BB.length d < block
                            then pure (acc <> d)
                            else inner (acc <> d)
     in
        inner mempty

httpError err = BBC.pack "HTTP/1.1 400 Bad Request\r\n\r\n" <> err

http404 _ = BBC.pack "HTTP/1.1 404 Not Found\r\n\r\n"

httpOk _ = "HTTP/1.1 200 OK\r\n\r\n"

handleHttpRequest req@HttpRequest{httpVerb, requestPath} =
    if requestPath == BBC.pack "/"
        then httpOk req
        else http404 req

handleRequestResult (Right httpRequest) = handleHttpRequest httpRequest
handleRequestResult (Left err) = httpError err

main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
        port = "4221"

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port

    serve (Host host) port $ \(serverSocket, _serverAddr) -> do
        BLC.putStrLn "Accepted connection"
        recvData <- recvAll serverSocket
        BBC.putStrLn recvData
        let res = handleRequestResult $ parseHttpHeader recvData
        BBC.putStrLn res
        send serverSocket res
