{-# LANGUAGE OverloadedStrings #-}

module HttpResponse (Status (..), ContentType (..), HttpResponse (..), httpResponse) where

import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC

data HttpResponse = HttpResponse
    { version :: BL.ByteString
    , status :: Status
    , contentType :: ContentType
    , body :: Maybe BL.ByteString
    }
    deriving (Show)

httpResponse :: HttpResponse -> BLC.ByteString
httpResponse HttpResponse{version, status, contentType, body} =
    version
        <> space
        <> statusToString status
        <> endOfLine
        <> contentTypeToString contentType
        <> endOfLine
        <> handleBody body
        <> endOfLine

data Status
    = Ok
    | Forbidden
    | BadRequest
    | NotFound
    | Created
    | NotAcceptable
    | NoContent
    deriving (Show)

statusToString :: Status -> BLC.ByteString
statusToString status = case status of
    Ok -> "200 OK"
    Forbidden -> "403 Forbidden"
    BadRequest -> "400 Bad Request"
    NotFound -> "404 Not Found"
    Created -> "201 Created"
    NotAcceptable -> "406 Not Acceptable"
    NoContent -> "204 No Content"

data ContentType
    = TextPlain
    | OctetStream
    | Html
    deriving (Show)

contentTypeToString :: ContentType -> BLC.ByteString
contentTypeToString contentType =
    "Content-Type: " <> case contentType of
        TextPlain -> "text/plain"
        Html -> "text/html"
        OctetStream -> "application/octet-stream"

endOfLine :: BLC.ByteString
endOfLine = BLC.pack "\r\n"

space :: BLC.ByteString
space = BLC.pack " "

contentLenght :: BLC.ByteString -> BLC.ByteString
contentLenght body = BLC.pack $ "Content-Length: " ++ show (BLC.length body)

handleBody :: Maybe BLC.ByteString -> BLC.ByteString
handleBody Nothing = endOfLine <> endOfLine
handleBody (Just body) = contentLenght body <> endOfLine <> endOfLine <> body
