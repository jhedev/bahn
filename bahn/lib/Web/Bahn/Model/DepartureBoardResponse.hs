{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.DepartureBoardResponse
    ( DepartureBoardResponse (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.DepartureBoard


data DepartureBoardResponse = DepartureBoardResponse
    { DepartureBoard :: DepartureBoard
    } deriving (Show, Eq, Generic)

instance FromJSON DepartureBoardResponse
instance ToJSON DepartureBoardResponse
instance Arbitrary DepartureBoardResponse where
    arbitrary = DepartureBoardResponse <$> arbitrary
