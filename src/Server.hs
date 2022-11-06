{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as BSU
import Data.Either                         (fromRight)
import Data.Maybe                          (fromJust)
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser
import Data.Text (Text)
import Database.Redis


-- --------------------------------------------------------------------------------
-- API type

type ReqApi = "requests" :>
  (    Get '[JSON] [ReqUrlWithId]
  :<|> ReqBody '[JSON] ReqUrl :> PostNoContent
  :<|> Capture "requestId" Int :> DeleteNoContent
  )

-- --------------------------------------------------------------------------------
-- Data types 

newtype ReqUrl = ReqUrl
  { getReqUrl :: String
  } deriving (Show, Generic)

instance FromJSON ReqUrl

data ReqUrlWithId = ReqUrlWithId
  { id     :: Integer
  , reqUrl :: String
  } deriving Generic

instance ToJSON ReqUrlWithId

-- --------------------------------------------------------------------------------
-- Handlers

server :: Server ReqApi
server = requests
    :<|> postRequest
    :<|> deleteRequest

-- --------------------------------------------------------------------------------
-- GET /requests

requests :: Handler [ReqUrlWithId]
requests = do
  conn <- liftIO $ checkedConnect defaultConnectInfo

  redisAction <- liftIO (runRedis conn $ do
    liftIO $ print "Getting data..."
    -- get all hash keys
    let key = "reqarr"
    wrappedMembers <- smembers key
    let hashKeys = fromRight [] wrappedMembers
        fieldId  = "id"
        fieldUrl = "reqUrl"
    
    -- return list of ReqUrlWithId
    return $ getHashData hashKeys [])

  reqUrls <- liftIO $ runRedis conn redisAction
  liftIO $ putStrLn $ "> GET successful (" ++ show (length reqUrls) ++ " items fetched)"
  return reqUrls

getHashData :: [BS.ByteString] -> [ReqUrlWithId] -> Redis [ReqUrlWithId]
getHashData [] reqs = return reqs
getHashData (x : xs) reqs = do
  vals <- hmget x ["id", "reqUrl"]
  case vals of
    Left reply -> error "some error occurred..."
    Right mbs  -> do
      let unwrapped = map fromJust mbs
          id        = read $ BSU.toString $ head unwrapped
          url       = BSU.toString $ last unwrapped
          reqUrl    = ReqUrlWithId id url
      getHashData xs (reqUrl : reqs)

-- --------------------------------------------------------------------------------
-- POST /requests

postRequest :: ReqUrl -> Handler NoContent
postRequest req = do
  conn <- liftIO $ checkedConnect defaultConnectInfo

  liftIO $ runRedis conn $ do
    -- get set members
    let key = "reqarr"
    wrappedMembers <- smembers key

    -- get a unique id
    let members  = fromRight [] wrappedMembers
    nextIdInt <- getUniqueId $ length members + 1

    -- set up data
    let nextId   = toBs nextIdInt
        hashKey  = key +++ ":" +++ nextId
        hashData = [ ("id", nextId), ("reqUrl", toBs . getReqUrl $ req) ]

    -- save hash and add hash to set
    hmset hashKey hashData
    sadd key [hashKey]
    return ()

  liftIO $ putStrLn $ "> POST successful (" ++ show req ++ ")"
  return NoContent

-- calculate a unique id
getUniqueId :: Int -> Redis Int
getUniqueId id = do
  let key = "reqarr"
      hkey = key +++ ":" +++ toBs id
  wrappedBool <- sismember key hkey
  if fromRight False wrappedBool
    then getUniqueId (id + 1)
    else return id

-- --------------------------------------------------------------------------------
-- DELETE /requests/:requestId

deleteRequest :: Int -> Handler NoContent
deleteRequest reqId = do
  conn <- liftIO $ checkedConnect defaultConnectInfo

  liftIO $ runRedis conn $ do
    let key    = "reqarr"
        remKey = key +++ ":" +++ toBs reqId

    -- confirm remKey is a member of the set
    wrappedBool <- sismember key remKey
    let isMember = fromRight False wrappedBool
    if not isMember then return ()
    else do
      -- TODO: Check wrappedNumRemoved
      wrappedNumRemoved <- srem key [remKey]
      -- remove hash from db
      del [remKey]
      return ()

  liftIO $ putStrLn $ "> DELETE successful (ID " ++ show reqId ++ ")"
  return NoContent

-- --------------------------------------------------------------------------------
-- Sample data

sample1 :: [ReqUrlWithId]
sample1 =
  [ ReqUrlWithId 1 "https://cardano-mainnet.blockfrost.io/api/v0"
  , ReqUrlWithId 2 "https://cardano-testnet.blockfrost.io/api/v0"
  , ReqUrlWithId 3 "https://cardano-preprod.blockfrost.io/api/v0"
  , ReqUrlWithId 4 "https://cardano-preview.blockfrost.io/api/v0"
  , ReqUrlWithId 5 "https://ipfs.blockfrost.io/api/v0"
  , ReqUrlWithId 6 "https://milkomeda-mainnet.blockfrost.io/api/v0"
  , ReqUrlWithId 7 "https://milkomeda-testnet.blockfrost.io/api/v0"
  ]

-- --------------------------------------------------------------------------------
-- Server 

apiType :: Proxy ReqApi
apiType = Proxy

app :: Application
app = serve apiType server

main :: IO ()
main = run 8081 app

-- --------------------------------------------------------------------------------
-- Utils

(+++) = BS.append
infixr 5 +++

toBs :: Show a => a -> BS.ByteString
toBs = BSU.fromString . show
