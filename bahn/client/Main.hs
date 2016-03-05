{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Servant.API
import Servant.Client

import Data.List.Split (splitOn)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Control.Monad
import Web.Bahn.Model.Operator
import Web.Bahn.Model.Types
import Web.Bahn.Model.JourneyDetailRef
import Web.Bahn.Model.Operators
import Web.Bahn.Model.DepartureBoard
import Web.Bahn.Model.LocalTime
import Web.Bahn.Model.Stop
import Web.Bahn.Model.ArrivalBoard
import Web.Bahn.Model.LocalDate
import Web.Bahn.Model.LocationResponse
import Web.Bahn.Model.ArrivalBoardResponse
import Web.Bahn.Model.Name
import Web.Bahn.Model.Names
import Web.Bahn.Model.DepartureBoardResponse
import Web.Bahn.Model.JourneyDetail
import Web.Bahn.Model.Type
import Web.Bahn.Model.DepartureOrArrival
import Web.Bahn.Model.Note
import Web.Bahn.Model.Stops
import Web.Bahn.Model.JourneyDetailResponse
import Web.Bahn.Model.StopLocation
import Web.Bahn.Model.LocationList
import Web.Bahn.Model.Notes
import Web.Bahn.DefaultApi

-- userClient :: IO ()
-- userClient = do 
--     users <- sample' (arbitrary :: Gen String)
--     let user = last users
--     void . runEitherT $ do
--         getUserByName user >>= (liftIO . putStrLn . show)

main :: IO ()
main = putStrLn "Hello Server!"
