{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Bahn.Types (
    ArrivalBoard (..),
    ArrivalBoardResponse (..),
    DepartureBoard (..),
    DepartureBoardResponse (..),
    DepartureOrArrival (..),
    JourneyDetail (..),
    JourneyDetailRef (..),
    JourneyDetailResponse (..),
    LocalDate (..),
    LocalTime (..),
    LocationList (..),
    LocationResponse (..),
    Name (..),
    Names (..),
    Note (..),
    Notes (..),
    Operator (..),
    Operators (..),
    Stop (..),
    StopLocation (..),
    Stops (..),
    Type (..),
    Types (..),
    ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- |
data ArrivalBoard = ArrivalBoard
    { arrivalBoardArrival :: [DepartureOrArrival] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON ArrivalBoard where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "arrivalBoard")
instance ToJSON ArrivalBoard where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "arrivalBoard")

-- |
data ArrivalBoardResponse = ArrivalBoardResponse
    { arrivalBoardResponseDepartureBoard :: ArrivalBoard -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON ArrivalBoardResponse where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "arrivalBoardResponse")
instance ToJSON ArrivalBoardResponse where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "arrivalBoardResponse")

-- |
data DepartureBoard = DepartureBoard
    { departureBoardDeparture :: [DepartureOrArrival] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON DepartureBoard where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "departureBoard")
instance ToJSON DepartureBoard where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "departureBoard")

-- |
data DepartureBoardResponse = DepartureBoardResponse
    { departureBoardResponseDepartureBoard :: DepartureBoard -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON DepartureBoardResponse where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "departureBoardResponse")
instance ToJSON DepartureBoardResponse where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "departureBoardResponse")

-- |
data DepartureOrArrival = DepartureOrArrival
    { departureOrArrivalName :: Text -- ^
    , departureOrArrivalType_ :: Text -- ^
    , departureOrArrivalStopid :: Int -- ^
    , departureOrArrivalStop :: Text -- ^
    , departureOrArrivalTime :: LocalTime -- ^
    , departureOrArrivalDate :: LocalDate -- ^
    , departureOrArrivalDirection :: Text -- ^
    , departureOrArrivalTrack :: Text -- ^
    , departureOrArrivalJourneyDetailRef :: JourneyDetailRef -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON DepartureOrArrival where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "departureOrArrival")
instance ToJSON DepartureOrArrival where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "departureOrArrival")

-- |
data JourneyDetail = JourneyDetail
    { journeyDetailStops :: Stops -- ^
    , journeyDetailNames :: Names -- ^
    , journeyDetailTypes :: Types -- ^
    , journeyDetailOperators :: Operators -- ^
    , journeyDetailNotes :: Notes -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON JourneyDetail where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "journeyDetail")
instance ToJSON JourneyDetail where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "journeyDetail")

-- |
data JourneyDetailRef = JourneyDetailRef
    { journeyDetailRefRef :: Text -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON JourneyDetailRef where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "journeyDetailRef")
instance ToJSON JourneyDetailRef where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "journeyDetailRef")

-- |
data JourneyDetailResponse = JourneyDetailResponse
    { journeyDetailResponseJourneyDetail :: JourneyDetail -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON JourneyDetailResponse where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "journeyDetailResponse")
instance ToJSON JourneyDetailResponse where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "journeyDetailResponse")

-- |
newtype LocalDate = LocalDate Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- |
newtype LocalTime = LocalTime Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- |
data LocationList = LocationList
    { locationListStopLocation :: [StopLocation] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON LocationList where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "locationList")
instance ToJSON LocationList where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "locationList")

-- |
data LocationResponse = LocationResponse
    { locationResponseLocationList :: LocationList -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON LocationResponse where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "locationResponse")
instance ToJSON LocationResponse where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "locationResponse")

-- |
data Name = Name
    { nameName :: Text -- ^
    , nameRouteIdxFrom :: Int -- ^
    , nameRouteIdxTo :: Int -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Name where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "name")
instance ToJSON Name where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "name")

-- |
data Names = Names
    { namesName :: [Name] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Names where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "names")
instance ToJSON Names where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "names")

-- |
data Note = Note
    { noteKey :: Text -- ^
    , notePriority :: Int -- ^
    , noteRouteIdxFrom :: Int -- ^
    , noteRouteIdxTo :: Int -- ^
    , note'Dollar :: Text -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Note where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "note")
instance ToJSON Note where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "note")

-- |
data Notes = Notes
    { notesNote :: [Note] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Notes where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "notes")
instance ToJSON Notes where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "notes")

-- |
data Operator = Operator
    { operatorName :: Text -- ^
    , operatorRouteIdxFrom :: Int -- ^
    , operatorRouteIdxTo :: Int -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Operator where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "operator")
instance ToJSON Operator where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "operator")

-- |
data Operators = Operators
    { operatorsOperator :: [Operator] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Operators where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "operators")
instance ToJSON Operators where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "operators")

-- |
data Stop = Stop
    { stopId :: Int -- ^
    , stopName :: Text -- ^
    , stopLon :: Double -- ^
    , stopLat :: Double -- ^
    , stopRouteIdx :: Int -- ^
    , stopDepTime :: LocalTime -- ^
    , stopDepDate :: LocalDate -- ^
    , stopTrack :: Text -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Stop where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "stop")
instance ToJSON Stop where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "stop")

-- |
data StopLocation = StopLocation
    { stopLocationId :: Int -- ^
    , stopLocationName :: Text -- ^
    , stopLocationLon :: Double -- ^
    , stopLocationLat :: Double -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON StopLocation where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "stopLocation")
instance ToJSON StopLocation where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "stopLocation")

-- |
data Stops = Stops
    { stopsStop :: [Stop] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Stops where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "stops")
instance ToJSON Stops where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "stops")

-- |
data Type = Type
    { typeType_ :: Text -- ^
    , typeRouteIdxFrom :: Int -- ^
    , typeRouteIdxTo :: Int -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Type where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "type")
instance ToJSON Type where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "type")

-- |
data Types = Types
    { typesType :: [Type] -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON Types where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "types")
instance ToJSON Types where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "types")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("@", "'At"), ("!", "'Exclamation"), ("#", "'Hash"), ("$", "'Dollar"), ("%", "'Percent"), ("&", "'Ampersand"), ("*", "'Star"), ("+", "'Plus"), ("-", "'Dash"), (":", "'Colon"), ("|", "'Pipe"), ("<", "'LessThan"), ("=", "'Equal"), ("^", "'Caret"), (">", "'GreaterThan")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace
