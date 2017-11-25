{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.Linklater
import Network.Wai.Handler.Warp (run)
import Prelude
import Utils.Bass as B
import Scrapers.RobAllen

readSlackFile :: FilePath -> IO T.Text
readSlackFile filename = T.filter (/= '\n') . T.pack <$> Prelude.readFile filename
configIO :: IO Config
configIO = Config <$> (readSlackFile "hook")

-- robAllenResults :: IO Network.Linklater.Message
-- robAllenResults = do
--   basses <- allRABasses
--   return (messageOf [FormatAt user, FormatString (T.intercalate "\n\n". Prelude.map B.toText $ B.liftBasses basses)]) where
--     messageOf = FormattedMessage(EmojiIcon "gift") "bassbot" channel

scrapeCommand :: Command -> IO Network.Linklater.Message
scrapeCommand (Command "bassbot" user channel (Just text)) = do
  basses <- allRABasses
  return (messageOf [FormatAt user, FormatString (T.intercalate "\n\n". Prelude.map B.toText $ B.liftBasses basses)]) where
    messageOf = FormattedMessage(EmojiIcon "gift") "bassbot" channel

bassify :: Command -> IO T.Text
bassify command = do
  Prelude.putStrLn ("+ Incoming command: " <> show command)  
  message <- scrapeCommand command
  config <- configIO
  Prelude.putStrLn ("+ Outgoing message: " <> show (message))
  return ""
  -- case (debug, message) of
  --   (False,  m) -> do
  --     _ <- say m config
  --     return ""
  --   _ ->
  --     return ""
  -- where
  --   debug = False

main :: IO ()
main =  do
	Prelude.putStrLn ("+ Listening on port " <> show port)
	run port (slashSimple bassify)
	where 
		port = 3000