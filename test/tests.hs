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
import Data.ByteString.UTF8 as BSU


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

extract' :: Either Reply ([Maybe BS.ByteString]) -> Maybe [BS.ByteString]
extract' x = case x of
  Left _   -> Nothing
  Right ys -> Just $ filter (/= "") $ fmap (fromMaybe "") ys

-- --------------------------------------------------------------------------------
-- Redis tests 

-- setThenGet 
prop_setThenGet :: Connection -> Property
prop_setThenGet conn = monadicIO $ do
  -- Set up random data 
  bs1 <- pick genReadableByteString
  bs2 <- pick genReadableByteString
  run $ runSetThenGet conn [bs1, bs2]

runSetThenGet :: Connection -> [BS.ByteString] -> IO Bool
runSetThenGet conn xs = do
  runRedis conn $ do
    set "hello" (head xs)
    set "world" (last xs)
    hello <- get "hello"
    world <- get "world"
    flushdb
    let r1 = fromMaybe "" $ extract hello
    let r2 = fromMaybe "" $ extract world
    return $ r1 == head xs && r2 == last xs

-- counterIncrBy
prop_counterIncrBy :: Connection -> Property
prop_counterIncrBy conn = monadicIO $ do
  x <- pick $ elements [1..100 :: Integer]
  y <- pick $ elements [1..100 :: Integer]
  run $ runRedis conn $ do
    let k = "key"
    set k (BSU.fromString . show $ x) >> incrby k y
    bs <- get k
    flushdb
    let r  = fromMaybe "0" $ extract bs
    let r' = read (BSU.toString r) :: Integer
    return $ r' == x + y

-- msetThenMget
prop_msetThenMget :: Connection -> Property
prop_msetThenMget conn = monadicIO $ do
  integers <- pick $ concat <$> listOf1 (vector 2 :: Gen [Integer])
  run $ runRedis conn $ do
    -- convert integers to key and value bytestrings for redis
    let xs = map (\x -> (toBs x, toBs x)) integers
    mset xs
    ys <- mget $ map toBs integers
    flushdb
    -- convert the retrieved bytestrings into a list of integers
    let ys' = maybe [] (map $ read . fromBs) (extract' ys)
    return $ ys' == integers
  where
    toBs   = BSU.fromString . show
    fromBs = BSU.toString

-- --------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  quickCheck $ prop_setThenGet    conn
  quickCheck $ prop_counterIncrBy conn
  quickCheck $ prop_msetThenMget  conn
  return ()
