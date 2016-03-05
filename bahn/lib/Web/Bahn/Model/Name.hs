{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Name
    ( Name (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Name = Name
    { name :: String
    , routeIdxFrom :: Integer
    , routeIdxTo :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON Name
instance ToJSON Name
instance Arbitrary Name where
    arbitrary = Name <$> arbitrary <*> arbitrary <*> arbitrary
