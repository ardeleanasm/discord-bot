{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Main where
import Data.Text(unpack,pack) --TO BE REMOVED, JUST FOR DEBUG
import Data.Text.Internal
import qualified Data.Text.IO as TIO
import Discord
import Discord.Types
import qualified Discord.Requests as R
import System.Environment (getArgs,getProgName)
import System.Directory (doesFileExist)
import Data.Time
import Data.Maybe
import Control.Monad (when)
import Control.Lens
import Commands

-- | TODO: 
-- 1. Read token from config file
-- 2. Add bark,bribe,bef ( befriend with Eugleniu) commands. Eugleniu will count how many friends has in a file
-- 3. .bef and .bark will work only after some relatively big time intervals, compared to bribe. If bribed, it will respond with eyes
-- 4. uptime feature
-- 5. 

data BotState = BotState
    {
        _botCurrentTime :: Maybe UTCTime
    } deriving (Show)

makeLenses ''BotState

startCurrentTime::BotState
startCurrentTime = BotState Nothing

connect :: Text -> IO ()
connect token = do 
    userFacingError <- runDiscord $ def 
            { discordToken = token
            , discordOnStart = onDiscordStartEventHandler
            , discordOnEvent = onDiscordEventHandler 
            }
    TIO.putStrLn userFacingError

onDiscordStartEventHandler :: DiscordHandle -> IO ()
onDiscordStartEventHandler dis = do
    currentTime <- getCurrentTime

    _ <- return (set botCurrentTime (Just currentTime) startCurrentTime)
    TIO.putStrLn $ pack ( show (view botCurrentTime startCurrentTime))


newCurrentTime :: UTCTime -> IO BotState
newCurrentTime t = return (BotState (Just t))

onDiscordEventHandler :: DiscordHandle -> Event -> IO ()
onDiscordEventHandler dis event = case event of
    MessageCreate m -> execute CommandContext{handler = dis, message = m,startTime = fromJust (view botCurrentTime startCurrentTime)} 
    _ -> pure ()

parseArgument :: [String] -> IO (String)
parseArgument args = do 
    case args of
        [path] -> return (path)
        [] -> return ("")

main :: IO ()
main = do
    args <- getArgs
    path <- parseArgument args
    fileExists <- doesFileExist path
    if fileExists == True then do
        content <- TIO.readFile path
        connect content
    else do
        putStrLn "File does not exists"
