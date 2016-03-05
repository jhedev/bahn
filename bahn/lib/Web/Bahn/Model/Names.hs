{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Names
    ( Names (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.Name


data Names = Names
    { Name :: [Name]
    } deriving (Show, Eq, Generic)

instance FromJSON Names
instance ToJSON Names
instance Arbitrary Names where
    arbitrary = Names <$> arbitrary
