{-# LANGUAGE OverloadedStrings #-}

module HttpRequest (parseHttpHeader, HttpRequest (..), HttpQuery (..), HttpVerb (..), HttpEncoding (..)) where

import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (toLower)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (IsString)

-- GET / HTTP/1.1\r\n
-- Host: 127.0.0.1:4221\r\n
-- User-Agent: Wget/1.21.3\r\n
-- Accept: */*\r\n
-- Accept-Encoding: identity\r\n
-- Connection: Keep-Alive\r\n

data HttpVerb
    = GET
    | POST
    | PUT
    | DELETE
    deriving (Show, Eq)

type HttpHeaders = [(BL.ByteString, BL.ByteString)]
type HttpVersion = BL.ByteString
type HttpBody = BL.ByteString

data HttpQuery = HttpQuery
    { path :: [BL.ByteString]
    , query :: Maybe BL.ByteString
    }
    deriving (Show, Eq)

data HttpEncoding = Gzip
    deriving (Show, Eq)

data HttpRequest = HttpRequest
    { httpVerb :: HttpVerb
    , requestPath :: HttpQuery
    , requestVersion :: HttpVersion
    , headers :: HttpHeaders
    , acceptEncoding :: Maybe HttpEncoding
    , body :: HttpBody
    }
    deriving (Show, Eq)

parseAcceptEncoding :: HttpHeaders -> Maybe HttpEncoding
parseAcceptEncoding h = (listToMaybe . mapMaybe (f . BC.dropWhile dw) . tokenise "," . BL.toStrict) =<< lookup "accept-encoding" h
  where
    dw ' ' = True
    dw '\r' = True
    dw '\n' = True
    dw '\t' = True
    dw _ = False
    f "gzip" = Just Gzip
    f _ = Nothing

tokenise :: BC.ByteString -> BC.ByteString -> [BB.ByteString]
tokenise x y = h : if BB.null t then [] else tokenise x (BB.drop (BB.length x) t)
  where
    (h, t) = BB.breakSubstring x y

httpVerbFromString :: (Eq a, IsString a) => a -> Either BC.ByteString HttpVerb
httpVerbFromString str = case str of
    "GET" -> Right GET
    "POST" -> Right POST
    "PUT" -> Right PUT
    "DELETE" -> Right DELETE
    _ -> Left $ BC.pack "verb not supported"

parseQuery :: BC.ByteString -> Either BB.ByteString HttpQuery
parseQuery str =
    let
        (p, q) = BC.break (== '?') str
        pp = filter (not . BC.null) $ BC.pack "/" : BC.split '/' p
        hq = HttpQuery{path = map BL.fromStrict pp, query = BL.stripPrefix (BLC.pack "?") (BL.fromStrict q)}
     in
        Right hq

parseRequestLine :: BC.ByteString -> Either BB.ByteString (HttpVerb, HttpQuery, HttpVersion)
parseRequestLine reqLine =
    case parts of
        [verbRaw, pathRaw, version] -> do
            verb <- httpVerbFromString $ BC.unpack verbRaw
            path <- parseQuery pathRaw
            Right (verb, path, BL.fromStrict version)
        err -> Left $ BC.pack ("Invalid REQUEST LINE " ++ show err)
  where
    parts = filter (not . BB.null) $ tokenise (BC.pack " ") reqLine

parseHttpHeaderList :: [BC.ByteString] -> Either BC.ByteString HttpHeaders
parseHttpHeaderList = Right . map ((\a -> (f a, a !! 1)) . map BL.fromStrict . tokenise (BC.pack ": "))
  where
    f = BLC.pack . map toLower . BLC.unpack . head

breakLines :: BC.ByteString -> [BC.ByteString]
breakLines = filter (not . BB.null) . tokenise (BC.pack "\r\n")

parseHttpHeader :: BC.ByteString -> Either BC.ByteString HttpRequest
parseHttpHeader str =
    do
        let (header, bodyRaw) = BC.breakSubstring "\r\n\r\n" str
            body = BC.stripPrefix "\r\n\r\n" bodyRaw
            requestLines = breakLines header
        (verb, reqPath, reqVer) <- parseRequestLine $ head requestLines
        headers <- parseHttpHeaderList $ tail requestLines
        let encoding = parseAcceptEncoding headers
        let res =
                HttpRequest
                    { httpVerb = verb
                    , requestPath = reqPath
                    , requestVersion = reqVer
                    , headers = headers
                    , acceptEncoding = encoding
                    , body = maybe mempty BL.fromStrict body
                    }
        pure res
