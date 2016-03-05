{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Stops
    ( Stops (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.Stop


data Stops = Stops
    { Stop :: [Stop]
    } deriving (Show, Eq, Generic)

instance FromJSON Stops
instance ToJSON Stops
instance Arbitrary Stops where
    arbitrary = Stops <$> arbitrary
