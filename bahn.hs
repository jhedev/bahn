{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.URL
import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T          
import qualified Data.ByteString.Char8 as B
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor
import           System.Environment

data Travel = Travel {
                    date :: String,
                    departure :: String,
                    prognosis :: String,
                    duration :: String,
                    changes :: String,
                    product :: String
                }

instance Show Travel where
    show (Travel dat dep prog dur ch prod) = dat ++ " | " ++ dep ++ " | " ++ (show prog) ++ " | " ++ (show dur) ++ " | " ++ (show ch) ++ " | " ++ prod

baseUrl :: URL
baseUrl = URL (Absolute (Host (HTTP False) "reiseauskunft.bahn.de" Nothing)) "bin/query.exe/dn" [("start","1")]

cursorFor :: URL -> IO Cursor
cursorFor u = do
        let s = exportURL u
        page <- simpleHttp s
        return $ fromDocument $ parseLBS page

findStartNodes :: Cursor -> [Cursor]
findStartNodes = element "table" &// element "tr" >=> attributeIs "class" " firstrow" 

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
    let url = case args of 
                    [s,z] -> foldl (\u p -> add_param u p) baseUrl [("S",s),("Z",z)]
    cursor <- cursorFor url
    putStrLn "Date | Departure | Prognosis | Duration | Changes | Products"
    mapM_ (putStrLn . show) $ cursor $// (findStartNodes &| extractData)
