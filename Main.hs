{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Base
import           Data.List (intercalate)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Network.HTTP.Conduit (simpleHttp)
import           Network.URL
import           System.Environment
import           System.IO
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor


data Travel = Travel
  { date      :: String
  , departure :: String
  , prognosis :: String
  , duration  :: String
  , changes   :: String
  , product   :: String
  }

instance Show Travel where
    show Travel{..} = intercalate " | " [ date
                                        , departure
                                        , prognosis
                                        , duration
                                        , changes
                                        , product
                                        ]

baseUrl :: URL
baseUrl = URL (Absolute
               (Host (HTTP False) "reiseauskunft.bahn.de" Nothing))
          "bin/query.exe/dn" [("start","1")]

cursorFor :: URL -> IO Cursor
cursorFor u = do
    let s = exportURL u
    putStr "Fetching results..."
    aPage <- async $ simpleHttp s
    loop aPage
  where
    loop a = do
      maybePage <- poll a
      case maybePage of
        Nothing -> do
          putStr "."
          threadDelay 100000
          loop a
        Just result -> case result of
          Left e -> throw e
          Right page -> return $ fromDocument $ parseLBS page

findStartNodes :: Cursor -> [Cursor]
findStartNodes = element "table" &// element "tr" >=>
                                       attributeIs "class" " firstrow"

extractData :: Cursor -> Travel
extractData c = Travel date dep prog dur ch prod
    where
      classCursor cls = head $ c $// element "td" >=> attributeIs "class" cls
      toString = T.unpack . T.strip . T.concat
      date = toString $ classCursor "date" $// content
      dep = toString $ classCursor "time" $/ content
      prog = toString $ classCursor "time" $// element "span" &/ content
      dur = toString $ classCursor "duration lastrow" $/ content
      ch = toString $ classCursor "changes lastrow" $/ content
      prod = toString $ classCursor "products lastrow" $/ content

main :: IO()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering
    let url = case args of
          [s,z] -> foldl add_param baseUrl [("S",s),("Z",z)]
          _     -> error "Not enough or too much arguments."
    cursor <- cursorFor url
    putStr "\n"
    putStrLn "Date | Departure | Prognosis | Duration | Changes | Products"
    mapM_ print $ cursor $// (findStartNodes &| extractData)
