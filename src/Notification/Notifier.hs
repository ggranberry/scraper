import Scrapers.RobAllen
import Utils.Bass

main = do
    basses <- allRABasses
    print $ liftBasses $ (return . filter isMouse) =<< basses
