{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.General
    (
      keyExists
    , keyDelete
    , keyType
    , keys
    , keyRandom
    , keyRename
    , keyRenameNX
    , databaseSize
    , keyTimeToLive
    , keyExpire
    , keyExpireAt
    , keyPersist
    , keyMove
    , flushDB
    , flushAll
    , select
    , ping
--    , unwrapReply
    ) where

import           System.IO
import qualified Data.Text as T
import           Database.Redis.Internal
-- import           Data.Maybe

-- | Calls @EXISTS@ (<http://code.google.com/p/redis/wiki/ExistsCommand>)
-- | with a 'Text' argument
keyExists :: Handle
          -> Text            -- ^ key
          -> IO (Maybe RedisReply)
keyExists h key = request h ["EXISTS", key]

-- | Calls @DEL@ (<http://code.google.com/p/redis/wiki/DelCommand>) with a
-- | list of 'Text' arguments.
keyDelete :: Handle
          -> [Text]          -- ^ keys to delete
          -> IO (Maybe RedisReply)
keyDelete h ks = request h $ "DEL" : ks

-- | Calls @TYPE@ (<http://code.google.com/p/redis/wiki/TypeCommand>) with a
-- 'Text' argument.
keyType :: Handle
        -> Text -- ^ key
        -> IO (Maybe RedisReply)
keyType h key = request h ["TYPE", key]

-- | Calls @KEYS@ with a 'Text' argument
keys :: Handle
     -> Text -- ^ key pattern
     -> IO (Maybe RedisReply)
keys h pattern = request h ["KEYS", pattern]

-- | Calls @RANDOMKEY@ (<http://code.google.com/p/redis/wiki/RandomkeyCommand>)
keyRandom :: Handle
          -> IO (Maybe RedisReply)
keyRandom h = request h ["RANDOMKEY"]

-- | Calls @RENAME@ (<http://code.google.com/p/redis/wiki/RenameCommand>) with
-- 'String' arguments.
keyRename :: Handle
          -> Text  -- ^ old key name
          -> Text  -- ^ new key name
          -> IO (Maybe RedisReply)
keyRename h old new = request h ["RENAME", old, new]

-- | Calls  @RENAMENX@ (<http://code.google.com/p/redis/wiki/RenamenxCommand>)
-- with 'String' arguments.
keyRenameNX :: Handle
            -> Text  -- ^ old key name
            -> Text  -- ^ new key name
            -> IO (Maybe RedisReply)
keyRenameNX h old new = request h ["RENAMENX", old, new]

-- | DBSIZE
databaseSize :: Handle
             -> IO (Maybe RedisReply)
databaseSize h = request h ["SELECT"]

-- | EXPIRE
keyExpire :: Handle
          -> Text  -- ^ key
          -> Int     -- ^ number of seconds intil expiry
          -> IO (Maybe RedisReply)
keyExpire h key i = request h ["EXPIRE", key, T.pack $ show i]

-- | EXPIREAT
keyExpireAt :: Handle
            -> Text  -- ^ key
            -> Int     -- ^ unix time of expiry
            -> IO (Maybe RedisReply)
keyExpireAt h key i = request h ["EXPIREAT", key, T.pack $ show i]

-- | PERSIST
keyPersist :: Handle
           -> Text  -- ^ key
           -> IO (Maybe RedisReply)
keyPersist h key = request h ["PERSIST", key]

-- | TTL
keyTimeToLive :: Handle
              -> Text  -- ^ key
              -> IO (Maybe RedisReply)
keyTimeToLive h key = request h ["PERSIST", key]

-- | Calls @SELECT@ (<http://code.google.com/p/redis/wiki/SelectCommand>)
-- with an 'Int' argument
select :: Handle
       -> Int  -- ^ the database to switch to
       -> IO (Maybe RedisReply)
select h i = request h ["SELECT", T.pack $ show i]

-- | MOVE
keyMove :: Handle
        -> Text  -- ^ key
        -> Int     -- ^ the database to switch to
        -> IO (Maybe RedisReply)
keyMove h key i = request h ["SELECT", key, T.pack $ show i]

-- | FLUSHDB
flushDB :: Handle
        -> IO (Maybe RedisReply)
flushDB h = request h ["FLUSHDB"]

-- | FLUSHALL
flushAll :: Handle
         -> IO (Maybe RedisReply)
flushAll h = request h ["FLUSHALL"]

-- | The PING command should return PONG
ping :: Handle
     -> IO (Maybe RedisReply)
ping h = request h ["PING"]
