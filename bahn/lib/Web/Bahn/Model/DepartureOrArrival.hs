{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.DepartureOrArrival
    ( DepartureOrArrival (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.JourneyDetailRef
import Web.Bahn.Model.LocalDate
import Web.Bahn.Model.LocalTime


data DepartureOrArrival = DepartureOrArrival
    { name :: String
    , type_ :: String
    , stopid :: Integer
    , stop :: String
    , time :: LocalTime
    , date :: LocalDate
    , direction :: String
    , track :: String
    , JourneyDetailRef :: JourneyDetailRef
    } deriving (Show, Eq, Generic)

instance FromJSON DepartureOrArrival
instance ToJSON DepartureOrArrival
instance Arbitrary DepartureOrArrival where
    arbitrary = DepartureOrArrival <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
