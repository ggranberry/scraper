{-# LANGUAGE OverloadedStrings #-}
module Scrapers.RobAllen where

import Text.HTML.Scalpel
import Control.Applicative
import Network.Curl
import Data.Text as T
import Utils.Bass

allRABasses :: IO (Maybe [Bass])
allRABasses = scrapeURLWithOpts opts "https://www.roballenguitars.com/new-products/" basses
    where
    basses :: Scraper String [Bass]
    basses = chroots ("div" @: [hasClass "ProductList-meta"]) bass

    opts = [ CurlUserAgent "curl/7.9.8 (i686-pc-linux-gnu) libcurl 7.9.8 (OpenSSL 0.9.6b) (ipv6 enabled)" ]

    bass :: Scraper String Bass
    bass = do
        description <- text $ "h1" @: [hasClass "ProductList-title"]
        price       <- text $ "span" @: [hasClass "sqs-money-native"]
        return $ Bass (T.pack description) (parsePrice price)
    
    parsePrice :: String -> Price
    parsePrice = read . removePunc

    removePunc :: String -> String
    removePunc xs = [ x | x <- xs, x /= ',' ]
