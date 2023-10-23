{-# LANGUAGE OverloadedStrings #-}

module Http (process) where

import Config qualified
import Data.ByteString.Char8 qualified as BBC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (intercalate)
import HttpRequest qualified as HReq
import HttpResponse qualified as HRes
import System.Directory (doesFileExist)
import System.FilePath ((</>))

process :: Config.Config -> BBC.ByteString -> IO BLC.ByteString
process config input = HRes.httpResponse <$> handleRequestResult config (HReq.parseHttpHeader input)

httpVersion :: BLC.ByteString
httpVersion = "HTTP/1.1"

httpError :: BLC.ByteString -> HRes.HttpResponse
httpError err =
    HRes.HttpResponse
        { HRes.version = httpVersion
        , HRes.status = HRes.BadRequest
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Just err
        }

http404 :: p -> HRes.HttpResponse
http404 _ =
    HRes.HttpResponse
        { HRes.version = httpVersion
        , HRes.status = HRes.NotFound
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Nothing
        }

httpNotAcceptable :: p -> HRes.HttpResponse
httpNotAcceptable _ =
    HRes.HttpResponse
        { HRes.version = httpVersion
        , HRes.status = HRes.NotAcceptable
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Nothing
        }

httpOk :: p -> HRes.HttpResponse
httpOk _ =
    HRes.HttpResponse
        { HRes.version = httpVersion
        , HRes.status = HRes.Ok
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Nothing
        }

httpEcho :: HReq.HttpRequest -> [String] -> HRes.HttpResponse
httpEcho req echo =
    HRes.HttpResponse
        { HRes.version = HReq.requestVersion req
        , HRes.status = HRes.Ok
        , HRes.contentType = HRes.TextPlain
        , HRes.body = Just $ BLC.pack $ intercalate "/" echo
        }

httpUserAgent :: HReq.HttpRequest -> HRes.HttpResponse
httpUserAgent HReq.HttpRequest{HReq.requestVersion, HReq.headers} =
    HRes.HttpResponse
        { HRes.version = requestVersion
        , HRes.status = HRes.Ok
        , HRes.contentType = HRes.TextPlain
        , HRes.body = lookup "User-Agent" headers
        }

httpFilesGet :: p1 -> [Char] -> IO HRes.HttpResponse
httpFilesGet req fpath = do
    exits <- doesFileExist fpath
    if not exits
        then pure $ http404 req
        else processFile
  where
    processFile = do
        fileContent <- BLC.readFile fpath
        pure
            $ HRes.HttpResponse
                { HRes.version = httpVersion
                , HRes.status = HRes.Ok
                , HRes.contentType = HRes.OctetStream
                , HRes.body = Just fileContent
                }

httpFilesPost :: HReq.HttpRequest -> [Char] -> IO HRes.HttpResponse
httpFilesPost req fpath = do
    exits <- doesFileExist fpath
    if exits
        then pure $ httpNotAcceptable req
        else processFile
  where
    processFile = do
        let body = HReq.body req
        BLC.writeFile fpath body
        pure
            $ HRes.HttpResponse
                { HRes.version = httpVersion
                , HRes.status = HRes.Created
                , HRes.contentType = HRes.OctetStream
                , HRes.body = Nothing
                }

httpFiles :: HReq.HttpRequest -> [Char] -> Config.Config -> IO HRes.HttpResponse
httpFiles req file conf = case Config.directory conf of
    Nothing -> pure $ http404 req
    Just dir -> do
        -- dir is an absolute path
        let fpath = dir </> file
        processFile fpath
  where
    processFile fpath = case HReq.httpVerb req of
        HReq.GET -> httpFilesGet req fpath
        HReq.POST -> httpFilesPost req fpath
        _ -> pure $ http404 req

handleHttpPath :: Config.Config -> HReq.HttpRequest -> [String] -> IO HRes.HttpResponse
handleHttpPath conf req p =
    case p of
        ["/"] -> pure $ httpOk req
        ("/" : "echo" : echo) -> pure $ httpEcho req echo
        ("/" : "user-agent" : _) -> pure $ httpUserAgent req
        ("/" : "files" : file : _) -> httpFiles req file conf
        _ -> pure $ http404 req

handleHttpRequest :: Config.Config -> HReq.HttpRequest -> IO HRes.HttpResponse
handleHttpRequest conf req = handleHttpPath conf req $ map BLC.unpack $ HReq.path $ HReq.requestPath req

handleRequestResult :: Config.Config -> Either BBC.ByteString HReq.HttpRequest -> IO HRes.HttpResponse
handleRequestResult config (Right httpRequest) = handleHttpRequest config httpRequest
handleRequestResult _ (Left err) = pure $ httpError $ BLC.fromStrict err
