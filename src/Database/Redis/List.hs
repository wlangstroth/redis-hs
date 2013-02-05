-- | This module interacts with the list structure in Redis.

{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.List
    ( listRightPush
    , listLeftPush
    , listLength
    , listRange
    , listIndex
    , listRemove
    , listTrim
    , listSet
    , listHeadPop
    , listTailPop
    , listBlockHeadPop
    , listBlockTailPop
    , listRPopLPush
    ) where

import           System.IO
import qualified Data.Text as T
import           Database.Redis.Internal

-- | Calls @RPUSH@ with 'Text' arguments
listRightPush :: Handle
              -> Text  -- ^ key
              -> Text  -- ^ value
              -> IO (Maybe RedisReply)
listRightPush h key value = request h ["RPUSH", key, value]

-- | Calls @LPUSH@ with 'Text' arguments
listLeftPush :: Handle
             -> Text  -- ^ key
             -> Text  -- ^ value
             -> IO (Maybe RedisReply)
listLeftPush h key value = request h ["LPUSH", key, value]


-- | Calls @LLEN@ with a 'Text' argument
listLength :: Handle
           -> Text    -- ^ key
           -> IO (Maybe RedisReply)
listLength h key = request h ["LLEN", key]


-- | Calls @LRANGE@ with a 'Text' argument
listRange :: Handle
          -> Text     -- ^ key
          -> Int      -- ^ start
          -> Int      -- ^ end
          -> IO (Maybe RedisReply)
listRange h key start end =
    request h ["LRANGE", key, T.pack $ show start, T.pack $ show end]


------------------------------------------------------------------------------
-- | LTRIM
listTrim :: Handle
           -> Text   -- ^ key
           -> Int      -- ^ start
           -> Int      -- ^ end
           -> IO (Maybe RedisReply)
listTrim h key start end =
    request h ["LTRIM", key, T.pack $ show start, T.pack $ show end]


-- | Calls @LINDEX@ with 'Text' and 'Int' arguments
listIndex :: Handle
          -> Text    -- ^ key
          -> Int           -- ^ index
          -> IO (Maybe RedisReply)
listIndex h key index =
    request h ["LINDEX", key, T.pack $ show index]


------------------------------------------------------------------------------
-- LSET
listSet :: Handle
          -> Text   -- ^ key
          -> Int      -- ^ index
          -> Text   -- ^ value
          -> IO (Maybe RedisReply)
listSet h key index value =
    request h ["LSET", key, T.pack $ show index, value]


------------------------------------------------------------------------------
-- | Calls @LREM@ with 'Text' and 'Int' arguments.  This command deletes
-- values matching the @value@ parameter. A negative 'Int' argument deletes
-- starting at the tail and moving towards the head (or from right to left,
-- after the push commands). A positive argument deletes from left to right.
-- Zero deletes all the elements.  Returns the number of elements deleted
-- (which should match the number) or 0 on failure.
listRemove :: Handle
           -> Text   -- ^ key
           -> Int          -- ^ the number of items to delete (sign is direction)
           -> Text   -- ^ value
           -> IO (Maybe RedisReply)
listRemove h key num value =
    request h ["LREM", key, T.pack $ show num, value]


------------------------------------------------------------------------------
-- | LPOP
listHeadPop :: Handle
           -> Text   -- ^ key
           -> IO (Maybe RedisReply)
listHeadPop h key =
    request h ["LPOP", key]


-- | RPOP
listTailPop :: Handle
           -> Text   -- ^ key
           -> IO (Maybe RedisReply)
listTailPop h key =
    request h ["RPOP", key]


-- | BLPOP
listBlockHeadPop :: Handle
           -> Text   -- ^ key
           -> IO (Maybe RedisReply)
listBlockHeadPop h key =
    request h ["BLPOP", key]


-- | BRPOP
listBlockTailPop :: Handle
                 -> Text   -- ^ key
                 -> IO (Maybe RedisReply)
listBlockTailPop h key =
    request h ["BRPOP", key]


-- | RPOPLPUSH
listRPopLPush :: Handle
                 -> Text   -- ^ source key
                 -> Text   -- ^ destination key
                 -> IO (Maybe RedisReply)
listRPopLPush h source destination =
    request h ["RPOPLPUSH", source, destination]


