{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Types
    ( Types (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.Type


data Types = Types
    { Type :: [Type]
    } deriving (Show, Eq, Generic)

instance FromJSON Types
instance ToJSON Types
instance Arbitrary Types where
    arbitrary = Types <$> arbitrary
