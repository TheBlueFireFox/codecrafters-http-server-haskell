{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config qualified
import Data.ByteString.Lazy.Char8 qualified as BLC
import Http qualified
import Socket qualified
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
    argv <- getArgs

    let host = "127.0.0.1"
        port = "4221"
        config = Config.argParse argv

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port
    Socket.serve host port (Http.process config)
