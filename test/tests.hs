{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

module Main where

import           Control.Monad.IO.Class
import           Database.Redis
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as BSU
import           Data.Char                 (ord)
import           Data.Maybe                (maybe, fromMaybe, fromJust)
import           Data.Either               (fromRight)
import           GHC.Word
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Instances ()

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

testDatabase :: Integer
testDatabase = 7

extract :: Either Reply (Maybe a) -> Maybe a
extract x = case x of
  Left _  -> Nothing
  Right y -> maybe Nothing Just y

extract' :: Either Reply [Maybe BS.ByteString] -> Maybe [BS.ByteString]
extract' x = case x of
  Left _   -> Nothing
  Right ys -> Just $ filter (/= "") $ fmap (fromMaybe "") ys

extractBool :: Either Reply Bool -> Maybe Bool
extractBool x = case x of
  Left _  -> Nothing
  Right b -> Just b

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
    select testDatabase
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
    select testDatabase
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
    select testDatabase
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

-- existThenDelete
prop_existsThenDelete :: Connection -> Property
prop_existsThenDelete conn = monadicIO $ do
  bs <- pick genReadableByteString
  run $ runRedis conn $ do
    select testDatabase
    let key = "mykey"
    set key bs
    exists1 <- exists key
    del [key]
    exists2 <- exists key
    return $ getBool exists1 && not (getBool exists2)
  where
    getBool = fromJust . extractBool

-- rpushThenRpushx
prop_rpushThenRpushx :: Connection -> Property
prop_rpushThenRpushx conn = monadicIO $ do
  x  <- pick $ elements [1..100 :: Integer]
  xs <- pick $ listOf1 (vector 2 :: Gen [Integer])
  run $ runRedis conn $ do
    select testDatabase
    let key = "mylist"
    rpush key [toBs x]
    mapM_ (rpushx key . toBs) (concat xs)
    list <- lrange key 0 (-1)
    del [key]
    let list' = map fromBs $ fromRight [] list
    return $ list' == x : concat xs
  where
    toBs = BSU.fromString . show
    fromBs = read . BSU.toString

-- hmsetThenHmget
prop_hmsetThenHmget :: Connection -> Property
prop_hmsetThenHmget conn = monadicIO $ do
  key      <- pick genReadableByteString
  integers <- pick $ concat <$> listOf1 (vector 1 :: Gen [Integer])
  
  -- convert integers to a [(ByteString, ByteString)] with unique fields
  let fieldValues = strToBsTup $ addSuffix 1 $ map (\x -> (show x, show x)) integers
  let fields      = map fst fieldValues
  let values      = map snd fieldValues
  
  -- start redis environment
  run $ runRedis conn $ do
    select testDatabase
    hmset key fieldValues
    emBs <- hmget key fields
    flushdb
    let bs = fromMaybe [] (extract' emBs)
    return $ bs == values
  where
    strToBsTup :: [(String, String)] -> [(BS.ByteString, BS.ByteString)]
    strToBsTup xs = map (\(f, v) ->
      (BSU.fromString f, BSU.fromString v)) xs

-- helper function, run addSuffixSample to see function
addSuffix :: Int -> [(String, String)] -> [(String, String)]
addSuffix _ [] = []
addSuffix n ((f, v): xs) = (go f n, v) : addSuffix (n+1) xs
  where
    go f n = f ++ ":" ++ show n  

addSuffixSample :: [(String, String)]
addSuffixSample = addSuffix 1 sampleData
  where sampleData = [("field", "val"), ("field", "val"), ("field", "val")]

-- --------------------------------------------------------------------------------
-- Test summary:
--  > Create a hash
--  > Add the hash to a new set
--  > Check if the hash key is a member of the set

prop_addHashToSet :: Connection -> Property
prop_addHashToSet conn = monadicIO $ do
  hashKey <- pick genReadableByteString
  setKey  <- pick genReadableByteString
  let hashFieldVals = [("field1", "val1"), ("field2", "val2")]
  
  run $ runRedis conn $ do
    select testDatabase
    -- create hash
    hmset hashKey hashFieldVals
    -- create set
    sadd setKey [hashKey]
    -- get members from set
    wrappedBool <- sismember setKey hashKey
    flushdb
    -- return True if the hash is a member of the set
    let result = fromRight False wrappedBool
    return result

-- --------------------------------------------------------------------------------
-- Test summary:
--  > Create a hash and add it to a new set
--  > Confirm that the hash is a member of the set
--  > Remove the hash from the set and delete it
--  > Confirm that the hash and set have been removed 

prop_delHashFromSet :: Connection -> Property
prop_delHashFromSet conn = monadicIO $ do
  hashKey <- pick genReadableByteString
  setKey  <- pick genReadableByteString
  let hashFieldVals = [("field1", "val1"), ("field2", "val2")]
  
  run $ runRedis conn $ do
    select testDatabase
    -- add hash and set 
    hmset hashKey hashFieldVals
    sadd setKey [hashKey]
    -- check hash is member of set
    wrappedBool <- sismember setKey hashKey
    let isMember = fromRight False wrappedBool
    if not isMember then return False
    else do
        -- remove hash from set
        wrappedNumRemoved <- srem setKey [hashKey]
        wrappedMembers    <- smembers setKey
        -- delete hash
        del [hashKey]
        flushdb
        let membersList = fromRight [""] wrappedMembers
            numRemoved  = fromRight 99 wrappedNumRemoved
        return $ null membersList && numRemoved == 1

-- --------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  renderHeader "prop_setThenGet:"
  quickCheck $ prop_setThenGet       conn
  renderHeader "prop_counterIncrBy:"
  quickCheck $ prop_counterIncrBy    conn
  renderHeader "prop_msetThenMget:"
  quickCheck $ prop_msetThenMget     conn
  renderHeader "prop_existsThenDelete:"
  quickCheck $ prop_existsThenDelete conn
  renderHeader "prop_rpushThenRpushx:"
  quickCheck $ prop_rpushThenRpushx  conn
  renderHeader "prop_hmsetThenHmget:"
  quickCheck $ prop_hmsetThenHmget   conn
  renderHeader "prop_addHashToSet:"
  quickCheck $ prop_addHashToSet     conn
  renderHeader "prop_delHashFromSet:"
  quickCheck $ prop_delHashFromSet   conn
  return ()
  where
    renderHeader str = putStr $  str ++ " " ++ replicate (25 - length str) ' '

