module Main where


import Test.Framework (defaultMain, testGroup)

import qualified Database.Redis.General.Tests
import qualified Database.Redis.Connection.Tests
import qualified Database.Redis.String.Tests
import qualified Database.Redis.List.Tests
import qualified Database.Redis.Set.Tests
-- import qualified Database.Redis.Transaction.Tests


main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "Database.Redis.General.Tests"
                            Database.Redis.General.Tests.tests
                , testGroup "Database.Redis.Connection.Tests"
                            Database.Redis.Connection.Tests.tests
                , testGroup "Database.Redis.String.Tests"
                            Database.Redis.String.Tests.tests
                , testGroup "Database.Redis.List.Tests"
                            Database.Redis.List.Tests.tests
                , testGroup "Database.Redis.Set.Tests"
                            Database.Redis.Set.Tests.tests
                -- , testGroup "Database.Redis.Transaction.Tests"
                --             Database.Redis.Transaction.Tests.tests
                ]
