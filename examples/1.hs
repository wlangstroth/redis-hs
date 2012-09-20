{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Database.Redis

main = do
  con <- connect localhost defaultPort
  _ <- select con 0
  _ <- itemSet con "atest" "testy test test"
  tester <- itemGet con "atest"
  putStrLn $ (show . fromJust) tester
