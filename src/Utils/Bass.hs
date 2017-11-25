module Utils.Bass
  where

  import Data.Text

  type Description = Text
  type Price = Double
  data Bass = Bass Description Price deriving (Show, Eq)

  isMouse :: Bass -> Bool
  isMouse (Bass desc _)
    | pack "Deep" `isInfixOf` desc = True
    | otherwise               = False

  liftBasses :: Maybe [Bass] -> [Bass]
  liftBasses Nothing = []
  liftBasses (Just x) = x

  toText :: Bass -> Text
  toText (Bass desc price) = pack $ (unpack desc) ++ (show price)

  toString :: Bass -> String
  toString (Bass desc price) = (unpack desc) ++ (show price)