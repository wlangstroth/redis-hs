-- |  This  module  has  nothing  to do  with  Haskell  @Text@s;  the  Redis
-- Command  Reference (<http://code.google.com/p/redis/wiki/CommandReference>)
-- confusingly refers  to its simple  key/value pairing as strings,  even when
-- those strings can be incremented. Unfortunately,  I can't think of a better
-- name.

{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.String
    ( itemSet
    , itemSetNX
    , itemSetEX
    , itemGet
    , multiSet
    , multiGet
    , multiSetNX
    , increment
    , incrementBy
    , decrement
    , decrementBy
    , itemGetSet
    , itemAppend
    , substring
    ) where

import           System.IO
import qualified Data.Text as T
import           Database.Redis.Internal

-- | SET for Text input
itemSet :: Handle
        -> Text            -- ^ key to set
        -> Text            -- ^ value to set
        -> IO (Maybe RedisReply)
itemSet h key value = request h ["SET", key, value]

-- | GET for a 'Text' argument
itemGet :: Handle
        -> Text            -- ^ key of the value to return
        -> IO (Maybe RedisReply)
itemGet h key = request h ["GET", key]

-- | GETSET for @Text@ arguments
-- <http://code.google.com/p/redis/wiki/GetsetCommand>
itemGetSet :: Handle
           -> Text                -- ^ key of the value to return
           -> Text                -- ^ new value
           -> IO (Maybe RedisReply)
itemGetSet h key value = request h ["GETSET", key, value]

-- MGET
multiGet :: Handle
         -> [Text]              -- ^ keys from which to return a value
         -> IO (Maybe RedisReply)
multiGet h keys = request h $ "MGET" : keys

-- | SETNX for @Text@ arguments
-- <http://code.google.com/p/redis/wiki/SetnxCommand>
itemSetNX :: Handle
          -> Text                -- ^ key to set
          -> Text                -- ^ value to set
          -> IO (Maybe RedisReply)
itemSetNX h key value = request h ["SETNX", key, value]

-- | SETEX
itemSetEX :: Handle
          -> Text                -- ^ key to set
          -> Int                   -- ^ number of seconds until expiration
          -> Text                -- ^ value to set
          -> IO (Maybe RedisReply)
itemSetEX h key i value = request h ["SETEX", key, T.pack $ show i, value]

-- |  Calls  @MSET@ <http://code.google.com/p/redis/wiki/MsetCommand>  with  a
-- list of tuples (key, value)
multiSet :: Handle
         -> [(Text, Text)]    -- ^ key/value pairs to set
         -> IO (Maybe RedisReply)
multiSet h kvs = request h $ "MSET" : (pairsToList kvs)

-- | Calls @MSETNX@ (<http://code.google.com/p/redis/wiki/MsetnxCommand>) with
-- a list of tuples (key, value)
multiSetNX :: Handle
           -> [(Text, Text)]    -- ^ key/value pairs to set
           -> IO (Maybe RedisReply)
multiSetNX h kvs = request h $ "MSET" : (pairsToList kvs)

-- | INCR
increment :: Handle
          -> Text                 -- ^ key to increment
          -> IO (Maybe RedisReply)
increment h key = request h ["INCR", key]

-- | INCRBY
incrementBy :: Handle
            -> Text                 -- ^ key to increment
            -> Int
            -> IO (Maybe RedisReply)
incrementBy h key i = request h ["INCRBY", key, T.pack $ show i]

-- | DECR
decrement :: Handle
          -> Text                 -- ^ key to decrement
          -> IO (Maybe RedisReply)
decrement h key = request h ["DECR", key]


------------------------------------------------------------------------------
-- DECRBY
decrementBy :: Handle
            -> Text                 -- ^ key to decrement
            -> Int
            -> IO (Maybe RedisReply)
decrementBy h key i = request h ["DECRBY", key, T.pack $ show i]


------------------------------------------------------------------------------
-- | APPEND for @Text@ arguments
-- <http://code.google.com/p/redis/wiki/AppendCommand>
itemAppend :: Handle
           -> Text                -- ^ key to append to
           -> Text                -- ^ value to append
           -> IO (Maybe RedisReply)
itemAppend h key value = request h ["APPEND", key, value]


------------------------------------------------------------------------------
-- | SUBSTR
substring :: Handle
          -> Text                -- ^ key to append to
          -> Int                   -- ^ start position
          -> Int                   -- ^ end position
          -> IO (Maybe RedisReply)
substring h key start end =
    request h ["SUBSTRING", key, T.pack $ show start, T.pack $ show end]


