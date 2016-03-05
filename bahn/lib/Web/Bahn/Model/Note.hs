{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Note
    ( Note (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Note = Note
    { key :: String
    , priority :: Integer
    , routeIdxFrom :: Integer
    , routeIdxTo :: Integer
    , $ :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Note
instance ToJSON Note
instance Arbitrary Note where
    arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
