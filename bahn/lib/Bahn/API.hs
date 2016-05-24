{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleInstances, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveTraversable, FlexibleContexts, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=308 #-}
module Bahn.API (
  -- * Client and Server
  ServerConfig(..),
  DBFahrplanBackend,
  createDBFahrplanClient,
  runDBFahrplanServer,
  runDBFahrplanClient,
  runDBFahrplanClientWithManager,
  DBFahrplanClient,
  -- ** Servant
  DBFahrplanAPI,
  ) where

import Bahn.Types

import Data.Aeson (Value)
import Data.Coerce (coerce)
import Servant.API
import Servant (serve, ServantErr)
import Web.HttpApiData
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import Data.Text (Text)
import Servant.Common.BaseUrl(BaseUrl(..))
import Servant.Client (ServantError, client, Scheme(Http))
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class
import Data.Function ((&))
import GHC.Exts (IsString(..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import Servant.API.Verbs (Verb, StdMethod(..))
import Control.Monad.Except (ExceptT)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types.Method (methodOptions)

instance ReflectMethod 'OPTIONS where
  reflectMethod _ = methodOptions




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either Text b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> key <> " in form data"
    Just value -> parseQueryParam value

-- | Servant type-level API, generated from the Swagger spec for DBFahrplan.
type DBFahrplanAPI
    =    "arrivalBoard" :> QueryParam "format" Text :> QueryParam "lang" Text :> QueryParam "id" Text :> QueryParam "date" Text :> QueryParam "time" Text :> Verb 'GET 200 '[JSON] ArrivalBoardResponse -- 'arrivalBoardGet' route
    :<|> "departureBoard" :> QueryParam "format" Text :> QueryParam "lang" Text :> QueryParam "id" Text :> QueryParam "date" Text :> QueryParam "time" Text :> Verb 'GET 200 '[JSON] DepartureBoardResponse -- 'departureBoardGet' route
    :<|> "journeyDetail" :> QueryParam "format" Text :> QueryParam "lang" Text :> QueryParam "ref" Text :> Verb 'GET 200 '[JSON] JourneyDetailResponse -- 'journeyDetailGet' route
    :<|> "location.name" :> QueryParam "format" Text :> QueryParam "lang" Text :> QueryParam "input" Text :> Verb 'GET 200 '[JSON] LocationResponse -- 'locationNameGet' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig {
    configHost :: String,  -- ^ Hostname to serve on, e.g. "127.0.0.1"
    configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList { fromQueryList :: [a] }
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat = CommaSeparated -- ^ CSV format for multiple parameters.
                      | SpaceSeparated -- ^ Also called "SSV"
                      | TabSeparated -- ^ Also called "TSV"
                      | PipeSeparated -- ^ `value1|value2|value2`
                      | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
    parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
    parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
    parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
    parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
    parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
    toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
    toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
    toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
    toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
    toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for DBFahrplan.
-- The backend can be used both for the client and the server. The client generated from the DBFahrplan Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createDBFahrplanClient@). Alternatively, provided
-- a backend, the API can be served using @runDBFahrplanServer@.
data DBFahrplanBackend m = DBFahrplanBackend {
    arrivalBoardGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m ArrivalBoardResponse{- ^ Retrieves the station board for the given station. This method will return the next 20 arrivals (or less if not existing) from a given point in time. The service can only be called for stops/stations by using according ID retrieved by the location.name method. -},
    departureBoardGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m DepartureBoardResponse{- ^ Retrieves the station board for the given station. This method will return the next 20 departures (or less if not existing) from a given point in time. The service can only be called for stops/stations by using according ID retrieved by the location.name method. -},
    journeyDetailGet :: Maybe Text -> Maybe Text -> Maybe Text -> m JourneyDetailResponse{- ^ Delivers information about the complete route of a vehicle. This service can't be called directly but only by reference URLs in a result of a departureBoard request. It contains a list of all stops/stations of this journey including all departure and arrival times (with realtime data if available / not supported right now) and additional information like specific attributes about facilities and other texts. -},
    locationNameGet :: Maybe Text -> Maybe Text -> Maybe Text -> m LocationResponse{- ^ The location.name service can be used to perform a pattern matching of a user input and to retrieve a list of possible matches in the journey planner database. Possible matches might be stops/stations, points of interest and addresses. -}
  }

newtype DBFahrplanClient a = DBFahrplanClient { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a }
    deriving Functor

instance Applicative DBFahrplanClient where
    pure x = DBFahrplanClient (\_ _ -> pure x)
    (DBFahrplanClient f) <*> (DBFahrplanClient x) = DBFahrplanClient (\manager url -> f manager url <*> x manager url)

instance Monad DBFahrplanClient where
    (DBFahrplanClient a) >>= f = DBFahrplanClient (\manager url -> do
        value <- a manager url
        runClient (f value) manager url)

instance MonadIO DBFahrplanClient where
    liftIO io = DBFahrplanClient (\_ _ -> liftIO io)

createDBFahrplanClient :: DBFahrplanBackend DBFahrplanClient
createDBFahrplanClient = DBFahrplanBackend{..}
  where
    ((coerce -> arrivalBoardGet) :<|>
     (coerce -> departureBoardGet) :<|>
     (coerce -> journeyDetailGet) :<|>
     (coerce -> locationNameGet)) = client (Proxy :: Proxy DBFahrplanAPI)

-- | Run requests in the DBFahrplanClient monad.
runDBFahrplanClient :: ServerConfig -> DBFahrplanClient a -> ExceptT ServantError IO a
runDBFahrplanClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runDBFahrplanClientWithManager manager clientConfig cl

-- | Run requests in the DBFahrplanClient monad using a custom manager.
runDBFahrplanClientWithManager :: Manager -> ServerConfig -> DBFahrplanClient a -> ExceptT ServantError IO a
runDBFahrplanClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the DBFahrplan server at the provided host and port.
runDBFahrplanServer :: MonadIO m => ServerConfig -> DBFahrplanBackend (ExceptT ServantErr IO)  -> m ()
runDBFahrplanServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy DBFahrplanAPI) (serverFromBackend backend)

  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend DBFahrplanBackend{..} =
      (coerce arrivalBoardGet :<|>
       coerce departureBoardGet :<|>
       coerce journeyDetailGet :<|>
       coerce locationNameGet)
