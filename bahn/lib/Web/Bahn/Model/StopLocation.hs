{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.StopLocation
    ( StopLocation (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data StopLocation = StopLocation
    { id_ :: Integer
    , name :: String
    , lon :: Double
    , lat :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON StopLocation
instance ToJSON StopLocation
instance Arbitrary StopLocation where
    arbitrary = StopLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
