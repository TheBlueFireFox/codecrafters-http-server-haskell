{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BBC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.String (IsString)
import Debug.Trace (trace)
import HttpRequest qualified as HReq
import HttpResponse qualified as HRes
import Network.Simple.TCP

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

httpError :: BLC.ByteString -> HRes.HttpResponse
httpError err =
    HRes.HttpResponse
        { HRes.version = "HTTP/1.1"
        , HRes.status = HRes.BadRequest
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Just err
        }

http404 :: p -> HRes.HttpResponse
http404 _ =
    HRes.HttpResponse
        { HRes.version = "HTTP/1.1"
        , HRes.status = HRes.NotFound
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Nothing
        }

httpOk :: p -> HRes.HttpResponse
httpOk _ =
    HRes.HttpResponse
        { HRes.version = "HTTP/1.1"
        , HRes.status = HRes.Ok
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Nothing
        }

httpEcho echo =
    HRes.HttpResponse
        { HRes.version = "HTTP/1.1"
        , HRes.status = HRes.Ok
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Just $ BLC.pack $ concat echo
        }

handleHttpPath req p =
    case p of
        ["/"] -> httpOk req
        ("/" : "echo" : echo) -> httpEcho echo
        _ -> http404 req

handleHttpRequest :: HReq.HttpRequest -> HRes.HttpResponse
handleHttpRequest req = handleHttpPath req $ map BBC.unpack $ HReq.path $ HReq.requestPath req

handleRequestResult :: Either BBC.ByteString HReq.HttpRequest -> HRes.HttpResponse
handleRequestResult (Right httpRequest) = handleHttpRequest httpRequest
handleRequestResult (Left err) = httpError $ BLC.fromStrict err

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
        let req = HReq.parseHttpHeader recvData
        BLC.putStrLn $ BLC.pack $ show req
        let res = handleRequestResult req
        let sendRes = HRes.httpResponse res
        BLC.putStrLn sendRes
        send serverSocket $ BB.toStrict sendRes
