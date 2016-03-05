{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.LocalDate
    ( LocalDate (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data LocalDate = LocalDate
    { 
    } deriving (Show, Eq, Generic)

instance FromJSON LocalDate
instance ToJSON LocalDate
instance Arbitrary LocalDate where
    arbitrary = LocalDate <$> 
