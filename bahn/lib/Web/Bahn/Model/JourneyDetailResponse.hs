{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.JourneyDetailResponse
    ( JourneyDetailResponse (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.JourneyDetail


data JourneyDetailResponse = JourneyDetailResponse
    { JourneyDetail :: JourneyDetail
    } deriving (Show, Eq, Generic)

instance FromJSON JourneyDetailResponse
instance ToJSON JourneyDetailResponse
instance Arbitrary JourneyDetailResponse where
    arbitrary = JourneyDetailResponse <$> arbitrary
