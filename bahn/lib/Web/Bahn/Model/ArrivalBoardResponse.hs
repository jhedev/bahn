{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Bahn.Model.ArrivalBoardResponse
    ( ArrivalBoardResponse (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Web.Bahn.Model.ArrivalBoard


data ArrivalBoardResponse = ArrivalBoardResponse
    { DepartureBoard :: ArrivalBoard
    } deriving (Show, Eq, Generic)

instance FromJSON ArrivalBoardResponse
instance ToJSON ArrivalBoardResponse
instance Arbitrary ArrivalBoardResponse where
    arbitrary = ArrivalBoardResponse <$> arbitrary
