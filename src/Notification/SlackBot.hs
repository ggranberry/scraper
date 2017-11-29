{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)  
import Control.Exception.Base
import Control.Monad  
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.Linklater
import Network.Wai.Handler.Warp (run)
import Utils.Bass as B
import Scrapers.RobAllen
import Network.Wreq

readSlackFile :: FilePath -> IO T.Text
readSlackFile filename = T.filter (/= '\n') . T.pack <$> Prelude.readFile filename
configIO :: IO Config
configIO = Config <$> (readSlackFile "hook")

scrapeCommand :: Command -> IO Network.Linklater.Message
scrapeCommand (Command "scraper" user channel (Just text)) = do
  basses <- allRABasses
  return (messageOf [FormatAt user, FormatString (T.intercalate "\n\n". Prelude.map B.toText $ B.liftBasses basses)])
  where
    messageOf = FormattedMessage(EmojiIcon ":stars2:") "scraper" channel

showLinkMessage :: Network.Linklater.Message -> T.Text
showLinkMessage (FormattedMessage a b c _) = T.pack $ (show a) ++ " " ++ (show b) ++ " " ++ (show c)

bassify :: Command -> IO T.Text
bassify command = do
  Prelude.putStrLn ("+ Incoming command: " <> show command)  
  message <- scrapeCommand command
  config <- configIO
  Prelude.putStrLn ("+ Outgoing message: " <> show (showLinkMessage message))
  case (debug, message) of
    (False,  m) -> do
      anEither <- runExceptT $ say m config
      case anEither of
        Left e -> putStrLn ("an error occurred! " <> show e)
        Right _ -> return ()
      return ""
    _ ->
      return ""
  where
    debug = False

main :: IO ()
main =  do
  Prelude.putStrLn ("+ Listening on port " <> show port)
  run port (slashSimple bassify)
  where 
    port = 3000