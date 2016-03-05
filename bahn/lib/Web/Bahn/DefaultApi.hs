{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bahn.DefaultApi (
      arrivalBoardGet
    , departureBoardGet
    , journeyDetailGet
    , locationNameGet
    , proxyDefaultApi
    , DefaultApi
    ) where

import GHC.Generics
import Data.Proxy
import Servant.API
import Servant.Client
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Servant.Common.Text
import Data.List (intercalate)
import qualified Data.Text as T
import Utils
import Test.QuickCheck
import Web.Bahn.Model.ArrivalBoardResponse
import Web.Bahn.Model.DepartureBoardResponse
import Web.Bahn.Model.JourneyDetailResponse
import Web.Bahn.Model.LocationResponse






type DefaultApi = "arrivalBoard" :> QueryParam "format" String :> QueryParam "lang" String :> QueryParam "id" String :> QueryParam "date" String :> QueryParam "time" String :> Get '[JSON] ArrivalBoardResponse -- arrivalBoardGet
    :<|> "departureBoard" :> QueryParam "format" String :> QueryParam "lang" String :> QueryParam "id" String :> QueryParam "date" String :> QueryParam "time" String :> Get '[JSON] DepartureBoardResponse -- departureBoardGet
    :<|> "journeyDetail" :> QueryParam "format" String :> QueryParam "lang" String :> QueryParam "ref" String :> Get '[JSON] JourneyDetailResponse -- journeyDetailGet
    :<|> "location.name" :> QueryParam "format" String :> QueryParam "lang" String :> QueryParam "input" String :> Get '[JSON] LocationResponse -- locationNameGet

proxyDefaultApi :: Proxy DefaultApi
proxyDefaultApi = Proxy


serverPath :: String
serverPath = "https://open-api.bahn.de/bin/rest.exe"

parseHostPort :: String -> (String, Int)
parseHostPort path = (host,port)
    where
        authority = case parseURI path of
            Just x -> uriAuthority x
            _      -> Nothing
        (host, port) = case authority of
            Just y -> (uriRegName y, (getPort . uriPort) y)
            _      -> ("localhost", 8080)
        getPort p = case (length p) of
            0 -> 80
            _ -> (read . drop 1) p

(host, port) = parseHostPort serverPath

arrivalBoardGet
    :<|> departureBoardGet
    :<|> journeyDetailGet
    :<|> locationNameGet
    = client proxyDefaultApi $ BaseUrl Http host port
