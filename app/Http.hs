{-# LANGUAGE OverloadedStrings #-}
module Http (process) where

import Data.ByteString.Char8 qualified as BBC
import Data.ByteString.Lazy.Char8 qualified as BLC
import HttpRequest qualified as HReq
import HttpResponse qualified as HRes
import Data.List (intercalate)

process :: BBC.ByteString -> BLC.ByteString
process = HRes.httpResponse . handleRequestResult . HReq.parseHttpHeader

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

handleHttpPath :: HReq.HttpRequest -> [[Char]] -> HRes.HttpResponse
handleHttpPath req p =
    case p of
        ["/"] -> httpOk req
        ("/" : "echo" : echo) -> httpEcho req echo
        _ -> http404 req

handleHttpRequest :: HReq.HttpRequest -> HRes.HttpResponse
handleHttpRequest req = handleHttpPath req $ map BLC.unpack $ HReq.path $ HReq.requestPath req

handleRequestResult :: Either BBC.ByteString HReq.HttpRequest -> HRes.HttpResponse
handleRequestResult (Right httpRequest) = handleHttpRequest httpRequest
handleRequestResult (Left err) = httpError $ BLC.fromStrict err
