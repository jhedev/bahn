{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Operator
    ( Operator (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Operator = Operator
    { name :: String
    , routeIdxFrom :: Integer
    , routeIdxTo :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON Operator
instance ToJSON Operator
instance Arbitrary Operator where
    arbitrary = Operator <$> arbitrary <*> arbitrary <*> arbitrary
