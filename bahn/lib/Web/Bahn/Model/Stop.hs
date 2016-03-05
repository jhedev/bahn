{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Stop
    ( Stop (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.LocalDate
import Web.Bahn.Model.LocalTime


data Stop = Stop
    { id_ :: Integer
    , name :: String
    , lon :: Double
    , lat :: Double
    , routeIdx :: Integer
    , depTime :: LocalTime
    , depDate :: LocalDate
    , track :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Stop
instance ToJSON Stop
instance Arbitrary Stop where
    arbitrary = Stop <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
