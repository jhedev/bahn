{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.JourneyDetailRef
    ( JourneyDetailRef (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data JourneyDetailRef = JourneyDetailRef
    { ref :: String
    } deriving (Show, Eq, Generic)

instance FromJSON JourneyDetailRef
instance ToJSON JourneyDetailRef
instance Arbitrary JourneyDetailRef where
    arbitrary = JourneyDetailRef <$> arbitrary
