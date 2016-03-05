{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.ArrivalBoard
    ( ArrivalBoard (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.DepartureOrArrival


data ArrivalBoard = ArrivalBoard
    { Arrival :: [DepartureOrArrival]
    } deriving (Show, Eq, Generic)

instance FromJSON ArrivalBoard
instance ToJSON ArrivalBoard
instance Arbitrary ArrivalBoard where
    arbitrary = ArrivalBoard <$> arbitrary
