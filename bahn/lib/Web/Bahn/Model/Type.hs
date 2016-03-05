{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Type
    ( Type (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Type = Type
    { type_ :: String
    , routeIdxFrom :: Integer
    , routeIdxTo :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON Type
instance ToJSON Type
instance Arbitrary Type where
    arbitrary = Type <$> arbitrary <*> arbitrary <*> arbitrary
