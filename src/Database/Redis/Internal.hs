{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Internal
    ( RedisReply(..)
    , Text
    , toInt
    , request
    , crlf
    , pairsToList
    ) where

import           Data.List (intersperse)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.IO

data RedisReply = RedisSingle Text
                | RedisError Text
                | RedisInteger Int
                | RedisBulk [Maybe RedisReply]
                  deriving (Eq)

instance Show RedisReply where
  show (RedisSingle x) = T.unpack x
  show (RedisInteger x) = show x
  show (RedisError x) = "Error: " ++ (T.unpack x)
  show (RedisBulk xs) = join xs
      where join = concat . intersperse ", " . map (show . fromJust)

-- | Sends the request
send :: Handle
     -> Text    -- ^ the request
     -> IO (Maybe RedisReply)
send h req = T.hPutStr h req >> T.hPutStr h crlf >> processReply h

-- | Formats and sends the request
request :: Handle
        -> [Text]  -- ^ list of requests
        -> IO (Maybe RedisReply)
request _ [] = return $ Just (RedisInteger 0)
request h commandList = send h $
    T.concat [ bulkLength commandList
              , crlf
              , sendCommands commandList
              ]

-- | Send commands
sendCommands :: [Text] -> Text
sendCommands [] = " "
sendCommands (c:cs) =
    T.append argLines $ sendCommands cs
  where
    argLines = T.concat [argLength c, crlf, c, crlf]

-- | Length of the bulk command
bulkLength :: [Text] -> Text
bulkLength cmds = T.concat ["*", T.pack $ show $ length cmds]

argLength :: Text -> Text
argLength arg = T.concat ["$", T.pack $ show $ T.length arg]

-- | The main  processing function. See the guide to Redis responses here:
-- <http://code.google.com/p/redis/wiki/ProtocolSpecification>
processReply :: Handle -> IO (Maybe RedisReply)
processReply h = do
    reply <- fmap T.strip $ T.hGetLine h
    case T.uncons reply of
      Just ('+', rest) -> singleReply rest
      Just ('-', rest) -> errorReply rest
      Just (':', rest) -> integerReply rest
      Just ('$', rest) -> bulkReply rest
      Just ('*', rest) -> multiBulkReply rest
      Just (_, _)      -> return $ Nothing
      Nothing          -> return $ Nothing
  where
    singleReply t = return $ Just $ RedisSingle t

    errorReply t = return $ Just $ RedisError t

    integerReply t = return $ Just $ RedisInteger $ toInt t

    bulkReply t = do
        body <- bulkBody $ toInt t
        return $ case body of
          Just x  -> Just (RedisBulk [Just $ RedisSingle x])
          _       -> Nothing

    bulkBody (-1) = return $ Nothing
    bulkBody size = do
        body <- T.hGetLine h
        let reply = T.take size body
        return $ Just reply

    multiBulkReply t = do
        bulks <- multiBulkReplies $ toInt t
        return $ Just $ RedisBulk bulks

    multiBulkReplies (-1) = return $ []
    multiBulkReplies 0    = return $ []
    multiBulkReplies n    = do
        r <- processReply h
        rs <- multiBulkReplies (n - 1)
        return $ r : rs

-- | Shorthand for \"\r\n\". Redis uses CRLF.
crlf :: Text
crlf = "\r\n"

-- | Integer value of Text
-- TODO: use R.decimal for this
toInt :: Text -> Int
toInt t = read $ T.unpack t :: Int

-- | Turns a list of pair tuples into a list
pairsToList :: [(a,a)] -> [a]
pairsToList [] = []
pairsToList ((a,b):rest) = a : b : pairsToList rest
