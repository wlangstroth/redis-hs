{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Connection.Tests
    ( tests ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import           Database.Redis

tests :: [Test]
tests = [ testCase "redis ping" pingTest ]

pingTest :: H.Assertion
pingTest = do
    con <- connect localhost defaultPort
    returning <- ping con
    disconnect con
    H.assertEqual "ping" (Just $ RedisSingle "PONG") returning
