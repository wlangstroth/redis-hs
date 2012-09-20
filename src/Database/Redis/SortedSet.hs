{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.SortedSet
    ( zSetAdd
    , zSetRemove
    , zSetIncrementBy
    , zSetRank
    , zSetReverseRank
    , zSetRange
    , zSetReverseRange
    , zSetRangeByScore
    , zSetCount
    , zSetCardinality
    , zSetScore
    , zSetRemRangeByRank
    , zSetRemRangeByScore
    , zSetUnionStore
    , zSetInterStore
    ) where

import           System.IO
import qualified Data.Text as T
import           Database.Redis.Internal

-- | ZADD
zSetAdd :: Handle
        -> Text  -- ^ key
        -> Text  -- ^ score, which can be a string float in Redis
        -> Text  -- ^ member
        -> IO (Maybe RedisReply)
zSetAdd h k s m = request h ["ZADD", k, s, m]

-- | ZREM
zSetRemove :: Handle
           -> Text  -- ^ key
           -> IO (Maybe RedisReply)
zSetRemove h k = request h ["ZREM", k]

-- | ZINCRBY
zSetIncrementBy :: Handle
                -> Text  -- ^ key
                -> Int     -- ^ amount to increment
                -> Text  -- ^ member
                -> IO (Maybe RedisReply)
zSetIncrementBy h k i m = request h ["ZINCRBY", k, T.pack $ show i, m]

-- | ZRANK
zSetRank :: Handle
         -> Text  -- ^ key
         -> Text  -- ^ member
         -> IO (Maybe RedisReply)
zSetRank h k m = request h ["ZRANK", k, m]

-- | ZREVRANK
zSetReverseRank :: Handle
                -> Text  -- ^ key
                -> Text  -- ^ member
                -> IO (Maybe RedisReply)
zSetReverseRank h k m = request h ["ZREVRANK", k, m]

-- | ZRANGE (does not yet include WITHSCORES)
zSetRange :: Handle
                -> Text   -- ^ key
                -> Int      -- ^ start
                -> Int      -- ^ end
                -> IO (Maybe RedisReply)
zSetRange h k s e = request h ["ZRANGE", k, T.pack $ show s, T.pack $ show e]

-- | ZREVRANGE (does not yet include WITHSCORES)
zSetReverseRange :: Handle
                 -> Text  -- ^ key
                 -> Int      -- ^ start
                 -> Int      -- ^ end
                 -> IO (Maybe RedisReply)
zSetReverseRange h k s e = request h ["ZREVRANGE", k, T.pack $ show s, T.pack $ show e]

-- | ZRANGEBYSCORE
zSetRangeByScore :: Handle
                 -> Text   -- ^ key
                 -> Int      -- ^ mn
                 -> Int      -- ^ mx
                 -> IO (Maybe RedisReply)
zSetRangeByScore h k mn mx =
    request h ["ZRANGEBYSCORE", k, T.pack $ show mn, T.pack $ show mx]

-- | ZCOUNT
zSetCount :: Handle
          -> Text   -- ^ key
          -> Int      -- ^ mn
          -> Int      -- ^ mx
          -> IO (Maybe RedisReply)
zSetCount h k mn mx =
    request h ["ZCOUNT", k, T.pack $ show mn, T.pack $ show mx]

-- | ZCARD
zSetCardinality :: Handle
                -> Text  -- ^ key
                -> IO (Maybe RedisReply)
zSetCardinality h k = request h ["ZCARD", k]

-- | ZSCORE
zSetScore :: Handle
          -> Text  -- ^ key
          -> Text  -- ^ member
          -> IO (Maybe RedisReply)
zSetScore h k m = request h ["ZSCORE", k, m]


-- | ZREMRANGEBYRANK
zSetRemRangeByRank :: Handle
                   -> Text  -- ^ key
                   -> Text  -- ^ member
                   -> IO (Maybe RedisReply)
zSetRemRangeByRank h k m = request h ["ZREVRANK", k, m]

-- | ZREMRANGEBYSCORE
zSetRemRangeByScore :: Handle
                    -> Text   -- ^ key
                    -> Int      -- ^ start
                    -> Int      -- ^ end
                    -> IO (Maybe RedisReply)
zSetRemRangeByScore h k s e = request h ["ZREMRANGEBYSCORE", k, T.pack $ show s, T.pack $ show e]

-- | ZUNIONSTORE
zSetUnionStore :: Handle
                -> Text  -- ^ key
                -> Text  -- ^ member
                -> IO (Maybe RedisReply)
zSetUnionStore h k m = request h ["ZUNIONSTORE", k, m]


-- | ZINTERSTORE
zSetInterStore :: Handle
                -> Text  -- ^ key
                -> Text  -- ^ member
                -> IO (Maybe RedisReply)
zSetInterStore h k m = request h ["ZINTERSTORE", k, m]
