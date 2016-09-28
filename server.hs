{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Servant
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Monad.Except
import Data.Either

import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           Network.Wai.Handler.Warp

type ItemId = Integer
type Next = Event

-- fields
data Event = Event
  { title :: String
  , eid :: ItemId
  , desc :: String
  , video :: String
  , date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON Event
instance FromJSON Event

-- Events endpoint
type EventsApi = "events" :> Get '[JSON] [Event]

jsonFile :: FilePath
jsonFile = "events.json"

loadData :: IO [Event]
loadData = fmap (either error id . eitherDecode) (B.readFile jsonFile)

eventsApi :: Proxy EventsApi
eventsApi = Proxy

server :: Server EventsApi
server = liftIO loadData >>= return

app :: Application
app = serve eventsApi server

runTestServer :: Port -> IO ()
runTestServer port = run port app

main :: IO ()
main = runTestServer 8081


