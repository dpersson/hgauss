{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Lib
    ( someFunc
    ) where
import Data.Maybe
import Data.Monoid
import Data.Aeson
import Data.Time.Format
import Data.Time
import Control.Monad
import GHC.Generics
import System.Environment
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HTypes
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import Text.Read (readMaybe)


{-
$   http://learnyouahaskell.com/higher-order-functions#function-application
do  http://learnyouahaskell.com/a-fistful-of-monads#do-notation
-}


myHeader1 = (HTypes.hAccept, C8.pack "some data")
type TaskId = String

apiUrl :: String
apiUrl = "https://reporting.smartadserverapis.com"

apiToken :: String
-- apiToken = "Basic <add authentication key here>"

requestUrl :: String
-- requestUrl = "/<add network-id here>/reports/"

requestBuilder :: TaskId -> String
requestBuilder tid = apiUrl <> requestUrl <> tid

newtype SmartTime = SmartTime { dateTime :: UTCTime } deriving Show

instance ToJSON SmartTime where
    toJSON (SmartTime d) = toJSON $ object [
        "date" .= T.pack (str 2 d)]
        where
            str n = pad . show where
                pad s = replicate (n - length s) '0' ++ s

instance FromJSON SmartTime where
    parseJSON = withText "SmartTime" $ \t ->
        case parseTimeM True defaultTimeLocale "%FT%T" (T.unpack t) of
             Just d -> pure (SmartTime d)
             _      -> fail "could not parse Smart time"

data Report = Report {
    taskId              :: T.Text,
    status              :: T.Text,
    lastTaskInstance    :: TaskInstance,
    date                :: SmartTime
} deriving Show


data TaskInstance = TaskInstance {
    taskInstanceId   :: T.Text,
    instanceStatus   :: T.Text
} deriving Show

instance FromJSON Report where
    parseJSON = withObject "report" $ \o -> do
        taskId              <- o .: "taskId"
        status              <- o .: "status"    
        lastTaskInstance    <- o .: "lastTaskInstance"
        date                <- o .: "creationDateUTC"
        return Report{..}
  
instance ToJSON Report where
    toJSON Report{..} = object [
        "taskId"            .= taskId,
        "status"            .= status,
        "lastTaskInstance"  .= lastTaskInstance,
        "creationDateUTC"   .= date ]

instance FromJSON TaskInstance where
    parseJSON = withObject "taskInstance" $ \o -> do
        taskInstanceId <- o .: "taskInstanceId"
        instanceStatus <- o .: "instanceStatus"
        return TaskInstance{..}
    
instance ToJSON TaskInstance where
    toJSON TaskInstance{..} = object [
        "taskInstanceId" .= taskInstanceId,
        "instanceStatus" .= instanceStatus ]

getReport :: TaskId -> IO (Either String Report)
getReport tid = do
    initReq <- parseRequest $ requestBuilder tid
    let request = initReq
                   { method = "GET"
                    ,requestHeaders = [(HTypes.hAuthorization, C8.pack apiToken)]}

    manager <- newManager tlsManagerSettings
    res     <- httpLbs request manager
    return (eitherDecode $ responseBody res)

someFunc :: IO ()
someFunc = do
  response <- getReport "9D5F57D3-331A-47F8-BC90-6D2EC860D0AD"
  case response of
    Left err -> putStrLn err
    Right ts -> print ts
