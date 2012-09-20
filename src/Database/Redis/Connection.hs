-- | Module containing all the functions necessary to make a connection to
-- an active Redis server.

module Database.Redis.Connection
    ( defaultPort
    , localhost
    , connect
    , disconnect
    ) where

import           Network
import           System.IO

------------------------------------------------------------------------------
-- | The number @6379@ as a Network 'PortNumber'
defaultPort :: PortNumber
defaultPort = 6379 :: PortNumber

------------------------------------------------------------------------------
-- | The string @127.0.0.1@ as a Network 'HostName'
localhost :: HostName
localhost = "127.0.0.1" :: HostName

------------------------------------------------------------------------------
connect :: HostName
        -> PortNumber
        -> IO Handle  -- ^ handle used by all the redis-hs functions
connect host port =
    withSocketsDo $ do
      h <- connectTo host (PortNumber port)
      hSetBuffering h NoBuffering
      return h

------------------------------------------------------------------------------
disconnect :: Handle -> IO ()
disconnect h = hClose h

-- TODO: AUTH
