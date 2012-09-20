{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Set
    ( setAdd
    , setRemove
    , setPop
    , setMove
    , setCardinality
    , setIsMember
    , setIntersect
    , setIntersectStore
    , setUnion
    , setUnionStore
    , setDiff
    , setDiffStore
    , setMembers
    , setRandMember
    ) where

import           System.IO
import           Database.Redis.Internal

-- | SADD
setAdd :: Handle
       -> Text  -- ^ key
       -> Text  -- ^ value
       -> IO (Maybe RedisReply)
setAdd h key value = request h ["SADD", key, value]

-- | SREM
setRemove :: Handle
          -> Text  -- ^ key
          -> Text  -- ^ value
          -> IO (Maybe RedisReply)
setRemove h key value = request h ["SREM", key, value]

-- | SPOP
setPop :: Handle
       -> Text  -- ^ key
       -> IO (Maybe RedisReply)
setPop h key = request h ["SPOP", key]

-- | SMOVE
setMove :: Handle
        -> Text  -- ^ source key
        -> Text  -- ^ destination key
        -> Text  -- ^ member
        -> IO (Maybe RedisReply)
setMove h s d m = request h ["SMOVE", s, d, m]

-- | SCARD
setCardinality :: Handle
               -> Text  -- ^ key
               -> IO (Maybe RedisReply)
setCardinality h key = request h ["SCARD", key]

-- | SISMEMBER
setIsMember :: Handle
            -> Text  -- ^ key
            -> Text  -- ^ value
            -> IO (Maybe RedisReply)
setIsMember h key value = request h ["SISMEMBER", key, value]

-- | SINTER
setIntersect :: Handle
             -> [Text]        -- ^ keys for sets to intersect
             -> IO (Maybe RedisReply)
setIntersect h keys = request h $ "SINTER":keys

-- | SINTERSTORE
setIntersectStore :: Handle
                  -> Text          -- ^ destination key
                  -> [Text]        -- ^ keys for sets to intersect
                  -> IO (Maybe RedisReply)
setIntersectStore h dest keys = request h $ "SINTER":dest:keys

-- | SUNION
setUnion :: Handle
         -> [Text]        -- ^ set keys to union
         -> IO (Maybe RedisReply)
setUnion h keys = request h $ "SUNION":keys

-- | SUNIONSTORE
setUnionStore :: Handle
              -> Text          -- ^ destination key
              -> [Text]        -- ^ set keys to union
              -> IO (Maybe RedisReply)
setUnionStore h dest keys = request h $ "SUNIONSTORE":dest:keys

-- | SDIFF
setDiff :: Handle
        -> [Text]        -- ^ keys for sets to union
        -> IO (Maybe RedisReply)
setDiff h keys = request h $ "SDIFF":keys

-- | SDIFFSTORE
setDiffStore :: Handle
             -> Text          -- ^ destination key
             -> [Text]        -- ^ keys for sets to diff
             -> IO (Maybe RedisReply)
setDiffStore h dest keys = request h $ "SDIFFSTORE":dest:keys

-- | SMEMBERS
setMembers :: Handle
           -> Text -- ^ key
           -> IO (Maybe RedisReply)
setMembers h key = request h ["SMEMBERS", key]

-- | SRANDMEMBER
setRandMember :: Handle
              -> Text  -- ^ key
              -> IO (Maybe RedisReply)
setRandMember h key = request h ["SRANDMEMBER", key]
