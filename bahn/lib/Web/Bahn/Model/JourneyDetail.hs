{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.JourneyDetail
    ( JourneyDetail (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.Names
import Web.Bahn.Model.Notes
import Web.Bahn.Model.Operators
import Web.Bahn.Model.Stops
import Web.Bahn.Model.Types


data JourneyDetail = JourneyDetail
    { Stops :: Stops
    , Names :: Names
    , Types :: Types
    , Operators :: Operators
    , Notes :: Notes
    } deriving (Show, Eq, Generic)

instance FromJSON JourneyDetail
instance ToJSON JourneyDetail
instance Arbitrary JourneyDetail where
    arbitrary = JourneyDetail <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
