{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.DepartureBoard
    ( DepartureBoard (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.DepartureOrArrival


data DepartureBoard = DepartureBoard
    { Departure :: [DepartureOrArrival]
    } deriving (Show, Eq, Generic)

instance FromJSON DepartureBoard
instance ToJSON DepartureBoard
instance Arbitrary DepartureBoard where
    arbitrary = DepartureBoard <$> arbitrary
