{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class
import Database.Redis
import Data.Char (ord)
import Data.Maybe (maybe, fromMaybe)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString as BS
import Test.QuickCheck.Instances ()
import GHC.Word
import Data.ByteString.Builder (word8)

-- https://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

-- --------------------------------------------------------------------------------
-- Generators

genReadableByteString :: Gen BS.ByteString
genReadableByteString = do
  let chars  = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']
      word8s = fromIntegral <$> (ord <$> chars)
  ws <- listOf1 $ elements word8s
  return $ BS.pack ws

-- --------------------------------------------------------------------------------
-- Utils

extract :: Either Reply (Maybe BS.ByteString) -> Maybe BS.ByteString
extract x = case x of
  Left _  -> Nothing
  Right y -> maybe Nothing Just y

-- --------------------------------------------------------------------------------
-- Redis tests 

prop_test1 :: Connection -> Property
prop_test1 conn = monadicIO $ do
  -- Set up random data 
  bs1 <- pick genReadableByteString
  bs2 <- pick genReadableByteString
  res <- run $ run_test1 conn [bs1, bs2]
  return res

run_test1 :: Connection -> [BS.ByteString] -> IO Bool
run_test1 conn xs = do
  runRedis conn $ do
    set "hello" (head xs)
    set "world" (last xs)
    hello <- get "hello"
    world <- get "world"
    flushdb
    let r1 = fromMaybe "" $ extract hello
    let r2 = fromMaybe "" $ extract world
    return $ r1 == head xs && r2 == last xs

-- --------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  quickCheck $ prop_test1 conn
  return ()
