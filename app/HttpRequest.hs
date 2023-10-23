module HttpRequest (parseHttpHeader, HttpRequest (..), HttpQuery (..), HttpVerb (..)) where

import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BC

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

type HttpHeaders = [(BB.ByteString, BB.ByteString)]
type HttpVersion = BB.ByteString

data HttpQuery = HttpQuery
    { pathRaw :: BB.ByteString
    , path :: [BB.ByteString]
    , query :: Maybe BB.ByteString
    }
    deriving (Show, Eq)

data HttpRequest = HttpRequest
    { httpVerb :: HttpVerb
    , requestPath :: HttpQuery
    , requestVersion :: HttpVersion
    , headers :: HttpHeaders
    }
    deriving (Show, Eq)

tokenise :: BC.ByteString -> BC.ByteString -> [BC.ByteString]
tokenise x y = h : if BB.null t then [] else tokenise x (BB.drop (BB.length x) t)
  where
    (h, t) = BB.breakSubstring x y

httpVerbFromString :: String -> Either BC.ByteString HttpVerb
httpVerbFromString str = case str of
    "GET" -> Right GET
    "POST" -> Right POST
    "PUT" -> Right PUT
    "DELETE" -> Right DELETE
    _ -> Left $ BC.pack "verb not supported"

parseQuery :: BC.ByteString -> Either BC.ByteString HttpQuery
parseQuery str =
    let
        (p, q) = BC.break (== '?') str
        pp = filter (not . BC.null) $ BC.pack "/" : BC.split '/' p
     in
        Right
            $ HttpQuery
                { pathRaw = p
                , path = pp
                , query = BB.stripPrefix (BC.pack "?") q
                }

parseRequestLine :: BC.ByteString -> Either BC.ByteString (HttpVerb, HttpQuery, HttpVersion)
parseRequestLine reqLine =
    let
        parts = filter (not . BB.null) $ tokenise (BC.pack " ") reqLine
     in
        case parts of
            [verbRaw, pathRaw, version] -> do
                verb <- httpVerbFromString $ BC.unpack verbRaw
                path <- parseQuery pathRaw
                Right (verb, path, version)
            err -> Left $ BC.pack ("Invalid REQUEST LINE " ++ show err)

parseHttpHeaderList :: [BC.ByteString] -> Either BC.ByteString HttpHeaders
parseHttpHeaderList = Right . map ((\a -> (head a, a !! 1)) . tokenise (BC.pack ": "))

breakLines :: BC.ByteString -> [BC.ByteString]
breakLines = filter (not . BB.null) . tokenise (BC.pack "\r\n")

parseHttpHeader :: BC.ByteString -> Either BC.ByteString HttpRequest
parseHttpHeader str =
    do
        let requestLines = breakLines str
        (verb, reqPath, reqVer) <- parseRequestLine $ head requestLines
        headers <- parseHttpHeaderList $ tail requestLines
        let res =
                HttpRequest
                    { httpVerb = verb
                    , requestPath = reqPath
                    , requestVersion = reqVer
                    , headers = headers
                    }
        pure res
