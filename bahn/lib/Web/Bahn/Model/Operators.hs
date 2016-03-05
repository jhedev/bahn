{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Operators
    ( Operators (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.Operator


data Operators = Operators
    { Operator :: [Operator]
    } deriving (Show, Eq, Generic)

instance FromJSON Operators
instance ToJSON Operators
instance Arbitrary Operators where
    arbitrary = Operators <$> arbitrary
