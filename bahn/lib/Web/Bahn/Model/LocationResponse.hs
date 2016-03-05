{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.LocationResponse
    ( LocationResponse (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.LocationList


data LocationResponse = LocationResponse
    { LocationList :: LocationList
    } deriving (Show, Eq, Generic)

instance FromJSON LocationResponse
instance ToJSON LocationResponse
instance Arbitrary LocationResponse where
    arbitrary = LocationResponse <$> arbitrary
