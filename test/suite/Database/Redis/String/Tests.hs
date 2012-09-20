{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.String.Tests
    ( tests ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
-- import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic

import           Database.Redis

tests :: [Test]
tests = [ testCase "redis itemSet"      itemSetTest
        , testCase "redis itemGet"      itemGetTest
        ]

itemSetTest :: H.Assertion
itemSetTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    returning <- itemSet con "akey" "a value"
    _ <- keyDelete con ["akey"]
    disconnect con
    H.assertEqual "itemSet" (Just $ RedisSingle "OK") returning

itemGetTest :: H.Assertion
itemGetTest = do
    con <- connect localhost defaultPort
    _ <- select con 0
    _ <- itemSet con "bKey" "a value"
    returning <- itemGet con "bKey"
    _ <- keyDelete con ["bKey"]
    disconnect con
    H.assertEqual "itemGet" (Just $ RedisBulk [Just (RedisSingle "a value")]) returning
