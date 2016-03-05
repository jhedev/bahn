{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.LocationList
    ( LocationList (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.StopLocation


data LocationList = LocationList
    { StopLocation :: [StopLocation]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationList
instance ToJSON LocationList
instance Arbitrary LocationList where
    arbitrary = LocationList <$> arbitrary
