{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Hash
    ( hashSet
    , hashGet
    , hashMultiGet
    , hashMultiSet
    , hashIncrementBy
    , hashExists
    , hashDelete
    , hashLength
    , hashKeys
    , hashValues
    , hashGetAll
    ) where

import           System.IO
import qualified Data.Text as T
import           Database.Redis.Internal

-- | Calls @HSET@ with 'Text' arguments
hashSet :: Handle
        -> Text  -- ^ key
        -> Text  -- ^ field
        -> Text  -- ^ value
        -> IO (Maybe RedisReply)
hashSet h key field value = request h ["HSET", key, field, value]

-- | Calls @HGET@ with 'Text' arguments
hashGet :: Handle
        -> Text  -- ^ key
        -> Text  -- ^ field
        -> IO (Maybe RedisReply)
hashGet h key field = request h ["HSET", key, field]

-- |  Calls @HMGET@ <http://code.google.com/p/redis/wiki/HmgetCommand>  with a
-- list of 'Text's
hashMultiGet :: Handle
             -> Text                -- ^ key
             -> [Text]              -- ^ fields
             -> IO (Maybe RedisReply)
hashMultiGet h k hs = request h $ "HMGET" : k : hs

-- | Calls  @HMSET@ <http://code.google.com/p/redis/wiki/HmsetCommand>  with a
-- list of tuples (key, value)
hashMultiSet :: Handle
             -> Text                -- ^ key
             -> [(Text, Text)]    -- ^ key/value pairs to set
             -> IO (Maybe RedisReply)
hashMultiSet h k kvs = request h $ "HMSET" : k : (pairsToList kvs)

-- | Calls @HINCRBY@ with 'Text' and 'Int' arguments
hashIncrementBy :: Handle
                -> Text   -- ^ key
                -> Text   -- ^ field
                -> Int      -- ^ field
                -> IO (Maybe RedisReply)
hashIncrementBy h key field i =
    request h ["HINCRBY", key, field, T.pack $ show i]

-- | Calls @HEXISTS@ (<http://code.google.com/p/redis/wiki/HexistsCommand>)
-- with 'Text' arguments
hashExists :: Handle
           -> Text                -- ^ key
           -> Text                -- ^ field
           -> IO (Maybe RedisReply)
hashExists h key field = request h ["HEXISTS", key, field]

-- | Calls @HDEL@ (<http://code.google.com/p/redis/wiki/HdelCommand>)
-- with 'Text' arguments
hashDelete :: Handle
           -> Text                -- ^ key
           -> Text                -- ^ field
           -> IO (Maybe RedisReply)
hashDelete h key field = request h ["HDEL", key, field]

-- | Calls @HLEN@ (<http://code.google.com/p/redis/wiki/HlenCommand>)
-- with 'Text' arguments
hashLength :: Handle
           -> Text                -- ^ key
           -> Text                -- ^ field
           -> IO (Maybe RedisReply)
hashLength h key field = request h ["HLEN", key, field]

-- | Calls @HKEYS@ (<http://code.google.com/p/redis/wiki/HkeysCommand>)
-- with a 'Text' argument. N.B. despite its name, it returns fields.
hashKeys :: Handle
         -> Text                -- ^ key
         -> IO (Maybe RedisReply)
hashKeys h k = request h ["HKEYS", k]

-- | Calls @HVALS@ (<http://code.google.com/p/redis/wiki/HvalsCommand>)
-- with a 'Text' argument.
hashValues :: Handle
           -> Text                -- ^ key
           -> IO (Maybe RedisReply)
hashValues h k = request h ["HVALS", k]

-- | Calls @HGETALL@ (<http://code.google.com/p/redis/wiki/HgetallCommand>)
-- with a 'Text' argument.
hashGetAll :: Handle
           -> Text                -- ^ key
           -> IO (Maybe RedisReply)
hashGetAll h k = request h ["HGETALL", k]
