-- | This module  corresponds to the transaction commands  introduced in Redis
-- 2.0, explained at <http://code.google.com/p/redis/wiki/MultiExecCommand>

{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Transaction
    ( multi
    , exec
    ) where

import           System.IO
import           Database.Redis.Internal

-- | MULTI, begin a transaction
multi :: Handle
      -> IO (Maybe RedisReply)
multi h = request h ["MULTI"]

-- | EXEC, execute the transaction
exec :: Handle
     -> IO (Maybe RedisReply)
exec h = request h ["EXEC"]


