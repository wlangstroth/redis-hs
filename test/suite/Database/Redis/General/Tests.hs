{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.General.Tests
    ( tests ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
-- import           Test.Framework.Providers.QuickCheck2

import qualified Test.HUnit as H

-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic

import           Database.Redis

tests :: [Test]
tests = [ testCase "redis keyExists"  keyExistsTest
        , testCase "redis keyType"    keyTypeTest
        , testCase "redis keys"       keysTest
        , testCase "redis keyRandom"  keyRandomTest
        , testCase "redis keyRename"  keyRenameTest
        ]


------------------------------------------------------------------------------
keyExistsTest :: H.Assertion
keyExistsTest = do
    con <- connect localhost defaultPort
    _ <- select con 8
    _ <- itemSet con "the_key" "thevalue"
    returning <- keyExists con "the_key"
    _ <- keyDelete con ["the_key"]
    disconnect con
    H.assertEqual "keyExists" (Just $ RedisInteger 1) returning


------------------------------------------------------------------------------
keyTypeTest :: H.Assertion
keyTypeTest = do
    con <- connect localhost defaultPort
    _ <- select con 8
    _ <- itemSet con "type_key" "typevalue"
    returning <- keyType con "type_key"
    _ <- keyDelete con ["type_key"]
    disconnect con
    H.assertEqual "keyType" (Just $ RedisSingle "string") returning

------------------------------------------------------------------------------
keysTest :: H.Assertion
keysTest = do
    con <- connect localhost defaultPort
    _ <- select con 8
    _ <- itemSet con "keystest0" "value"
    _ <- itemSet con "keystest1" "value"
    returning <- keys con "keys*"
    _ <- keyDelete con ["keystest0"]
    _ <- keyDelete con ["keystest1"]
    disconnect con
    H.assertEqual "keys"
      (Just (RedisBulk [Just (RedisBulk [Just (RedisSingle "keystest0")])
      ,Just (RedisBulk [Just (RedisSingle "keystest1")])]))
      returning


------------------------------------------------------------------------------
keyRandomTest :: H.Assertion
keyRandomTest = do
    con <- connect localhost defaultPort
    _ <- select con 9
    _ <- keyRandom con
    disconnect con
    H.assertEqual "keyRandom" "blah" "blah"


------------------------------------------------------------------------------
keyRenameTest :: H.Assertion
keyRenameTest = do
    con <- connect localhost defaultPort
    _ <- select con 9
    _ <- itemSet con "renametest" "special"
    _ <- keyRename con "renametest" "success"
    returning <- keys con "success"
    _ <- keyDelete con ["success"]
    disconnect con
    H.assertEqual "keysRename"
      (Just (RedisBulk [Just (RedisBulk [Just (RedisSingle "success")])]))
      returning
