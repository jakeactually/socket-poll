import Control.Concurrent
import Control.Exception
import Data.Time.Format
import Data.Time.LocalTime
import Network.Simple.TCP
import System.Environment

main :: IO ()
main = do
    ip <- head <$> getArgs
    poll ip

poll :: String -> IO ()
poll ip = do
    either <- try $ ping ip :: IO (Either IOError ())
    stamp <- getStamp
    appendFile "log.txt" $ case either of
        Left e -> stamp ++ " " ++ show e ++ "\n"      
        Right () -> stamp ++ " on" ++ "\n"
    threadDelay $ 1000000 * 60 * 5
    poll ip

ping :: String -> IO ()
ping ip = connect ip "4040" (const $ pure ())

getStamp :: IO String
getStamp = formatTime defaultTimeLocale "%m/%d %H:%M" <$> getZonedTime
