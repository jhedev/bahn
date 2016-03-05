{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.LocalTime
    ( LocalTime (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data LocalTime = LocalTime
    { 
    } deriving (Show, Eq, Generic)

instance FromJSON LocalTime
instance ToJSON LocalTime
instance Arbitrary LocalTime where
    arbitrary = LocalTime <$> 
