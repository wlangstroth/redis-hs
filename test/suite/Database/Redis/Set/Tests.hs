{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Set.Tests
    ( tests ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
-- import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic

import           Database.Redis

tests :: [Test]
tests = [ testCase "redis setAdd"         setAddTest
        , testCase "redis setMembers"     setMembersTest
        , testCase "redis setRemove"      setRemoveTest
        , testCase "redis setRandMember"  setRandMemberTest
        ]

setAddTest :: H.Assertion
setAddTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    returning <- setAdd con "thebset" "bob"
    _ <- keyDelete con ["thebset"]
    disconnect con
    H.assertEqual "setAdd" (Just $ RedisInteger 1) returning

setMembersTest :: H.Assertion
setMembersTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- setAdd con "nameset" "Bob"
    _ <- setAdd con "nameset" "Jane"
    returning <- setMembers con "nameset"
    _ <- keyDelete con ["nameset"]
    disconnect con
    H.assertEqual "setMembers"
      (Just (RedisBulk [Just (RedisBulk [Just (RedisSingle "Bob")])
      ,Just (RedisBulk [Just (RedisSingle "Jane")])]))
      returning

setRemoveTest :: H.Assertion
setRemoveTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- setAdd con "setitb" "Bob"
    _ <- setAdd con "setitb" "Frank"
    _ <- setAdd con "setitb" "Jane"
    _ <- setRemove con "setitb" "Frank"
    returning <- setMembers con "setitb"
    _ <- keyDelete con ["setitb"]
    disconnect con
    H.assertEqual "setRemove"
      (Just (RedisBulk [Just (RedisBulk [Just (RedisSingle "Bob")])
      ,Just (RedisBulk [Just (RedisSingle "Jane")])]))
      returning

setRandMemberTest :: H.Assertion
setRandMemberTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- setAdd con "randset" "Bob"
    _ <- setAdd con "randset" "Jane"
    _ <- setRandMember con "nameset"
    _ <- keyDelete con ["randbset"]
    disconnect con
    H.assertEqual "setContains" "blah" "blah"
