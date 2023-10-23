{-# LANGUAGE OverloadedStrings #-}

module Socket (recvAll, serve) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BBC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Network.Simple.TCP qualified as Tcp

recvAll :: (MonadIO f) => Tcp.Socket -> f BBC.ByteString
recvAll sock = inner mempty
  where
    block = 1024
    inner acc = con acc =<< Tcp.recv sock block

    con acc Nothing = pure acc
    con acc (Just d)
        | BB.length d < block = pure (acc <> d)
        | otherwise = inner (acc <> d)

serve :: MonadIO m => Tcp.HostName -> Tcp.ServiceName -> (BBC.ByteString -> IO BLC.ByteString) -> m a
serve host port handler = Tcp.serve (Tcp.Host host) port $ \(serverSocket, _serverAddr) -> do
    BLC.putStrLn "Accepted connection"
    req <- recvAll serverSocket
    BBC.putStrLn req

    res <- handler req

    BLC.putStrLn res
    Tcp.send serverSocket $ BB.toStrict res
