{-# LANGUAGE OverloadedStrings #-}
module Database.Redis.List.Tests
    ( tests ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
-- import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic

import           Database.Redis


tests :: [Test]
tests = [ testCase "redis listRightPush"  listRightPushTest
        , testCase "redis listLeftPush"   listLeftPushTest
        , testCase "redis listIndex"      listIndexTest
        , testCase "redis listLength"     listLengthTest
        , testCase "redis listRange"      listRangeTest
        , testCase "redis listRemove"     listRemoveTest
        ]

listRightPushTest :: H.Assertion
listRightPushTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    returning <- listRightPush con "anotherlist" "theitem"
    _ <- keyDelete con ["anotherlist"]
    disconnect con
    H.assertBool "listRightPushB" $ case returning of
                                     Just (RedisInteger _) -> True
                                     _                     -> False

listLeftPushTest :: H.Assertion
listLeftPushTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    returning <- listLeftPush con "leftblist" "anotheritem"
    _ <- keyDelete con ["leftblist"]
    disconnect con
    H.assertBool "listLeftPush" $ case returning of
                                     Just (RedisInteger _) -> True
                                     _                     -> False

listLengthTest :: H.Assertion
listLengthTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- listRightPush con "lengthlist" "anitem"
    _ <- listRightPush con "lengthlist" "another"
    returning <- listLength con "lengthlist"
    _ <- keyDelete con ["lengthlist"]
    disconnect con
    H.assertEqual "listLengthB" (Just $ RedisInteger 2) returning

listIndexTest :: H.Assertion
listIndexTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- listRightPush con "indexblist" "theitem"
    returning <- listIndex con "indexblist" 0
    _ <- keyDelete con ["indexblist"]
    disconnect con
    H.assertEqual "listIndex"
      (Just $ RedisBulk [Just (RedisSingle "theitem")])
      returning

listRangeTest :: H.Assertion
listRangeTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- listRightPush con "ablist" "value0"
    _ <- listRightPush con "ablist" "value1"
    returning <- listRange con "ablist" 0 1
    _ <- keyDelete con ["ablist"]
    disconnect con
    H.assertEqual "listRange"
      (Just (RedisBulk [Just (RedisBulk [Just (RedisSingle "value0")])
      ,Just (RedisBulk [Just (RedisSingle "value1")])]))
      returning

listRemoveTest :: H.Assertion
listRemoveTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- listRightPush con "rlistb" "value"
    _ <- listRightPush con "rlistb" "value"
    returning <- listRemove con "rlistb" (-1) "value"
    _ <- keyDelete con ["rlistb"]
    disconnect con
    H.assertEqual "listRemove" (Just $ RedisInteger 1) returning
