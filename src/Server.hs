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
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
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
  { getReqUrl :: Text
  } deriving (Show, Generic)

instance FromJSON ReqUrl

data ReqUrlWithId = ReqUrlWithId
  { id     :: Integer
  , reqUrl :: Text
  } deriving Generic

instance ToJSON ReqUrlWithId

-- --------------------------------------------------------------------------------
-- Handlers

server :: Server ReqApi
server = requests
    :<|> postRequest
    :<|> deleteRequest

-- GET /requests
requests :: Handler [ReqUrlWithId]
requests = do
  liftIO $ putStrLn "> getting request data"
  return sample1

-- POST /requests
postRequest :: ReqUrl -> Handler NoContent
postRequest req = do
  liftIO $ putStrLn $ "> performing POST for " ++ show req
  return NoContent

-- DELETE /requests/requestId
deleteRequest :: Int -> Handler NoContent
deleteRequest reqId = do
  liftIO $ putStrLn $ "> performing DELETE with id " ++ show reqId
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
