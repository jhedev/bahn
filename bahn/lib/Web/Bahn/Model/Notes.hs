{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.Notes
    ( Notes (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.Note


data Notes = Notes
    { Note :: [Note]
    } deriving (Show, Eq, Generic)

instance FromJSON Notes
instance ToJSON Notes
instance Arbitrary Notes where
    arbitrary = Notes <$> arbitrary
