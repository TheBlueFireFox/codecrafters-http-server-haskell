module HttpRequest (parseHttpHeader, HttpRequest (..), HttpHeaders (..), HttpVerb (..)) where

import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (trace)

-- GET / HTTP/1.1\r\n
-- Host: 127.0.0.1:4221\r\n
-- User-Agent: Wget/1.21.3\r\n
-- Accept: */*\r\n
-- Accept-Encoding: identity\r\n
-- Connection: Keep-Alive\r\n

data HttpVerb
    = GET
    | POST
    deriving (Show, Eq)

type HttpHeaders = [(BB.ByteString, BB.ByteString)]

data HttpRequest = HttpRequest
    { httpVerb :: HttpVerb
    , requestPath :: BB.ByteString
    , requestVersion :: BB.ByteString
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
    _ -> Left $ BC.pack "verb not supported"

parseRequestLine :: BC.ByteString -> Either BC.ByteString (HttpVerb, BC.ByteString, BC.ByteString)
parseRequestLine reqLine =
    let
        parts = filter (not . BB.null) $ tokenise (BC.pack " ") reqLine
     in
        trace ("request " ++ show reqLine)
            $ case parts of
                [verbRaw, path, version] -> do
                    verb <- httpVerbFromString $ BC.unpack verbRaw
                    Right (verb, path, version)
                err -> Left $ BC.pack ("Invalid REQUEST LINE " ++ show err)

parseHttpHeader :: BC.ByteString -> Either BC.ByteString HttpRequest
parseHttpHeader str =
    let
        breakLines = tokenise $ BC.pack "\r\n"
     in
        do
            let l = breakLines str
            (verb, reqPath, reqVer) <- parseRequestLine $ head l
            pure
                $ HttpRequest
                    { httpVerb = verb
                    , requestPath = reqPath
                    , requestVersion = reqVer
                    , headers = []
                    }
