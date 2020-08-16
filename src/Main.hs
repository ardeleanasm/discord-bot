{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where
import Data.Text(unpack,pack) --TO BE REMOVED, JUST FOR DEBUG
import Data.Text.Internal
import qualified Data.Text.IO as TIO
import Discord
import Discord.Types
import qualified Discord.Requests as R
import System.Environment (getArgs,getProgName)
import System.Directory (doesFileExist,createDirectoryIfMissing,getAppUserDataDirectory)
import System.FilePath ((</>))
import Data.Time
import Data.Maybe
import Control.Monad (when)
import Control.Applicative
import Commands
import ConfigParser
-- | TODO: 
-- 2. Add bark,bribe,bef ( befriend with Eugleniu) commands. Eugleniu will count how many friends has in a file
-- 3. .bef and .bark will work only after some relatively big time intervals, compared to bribe. If bribed, it will respond with eyes
-- 5. 
-- 6. Implement config file in folder .discord-bot with each configuration per line. It will
-- contain also the token!
-- 7. config file is in JSON FORMAT!


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
    TIO.writeFile "/home/mihai/config_bot" (pack $ show currentTime)
    TIO.putStrLn (pack $ show currentTime)



onDiscordEventHandler :: DiscordHandle -> Event -> IO ()
onDiscordEventHandler dis event = case event of
    MessageCreate m -> execute CommandContext{handler = dis, message = m} 
    _ -> pure ()



main :: IO ()
main = do
    configDirectoryPath <- getAppUserDataDirectory "discord-bot"
    _ <- createDirectoryIfMissing False configDirectoryPath
    configFile <-return $ configDirectoryPath ++ "/config.json"
    configuration <- parseConfiguration configFile
    case configuration of 
        Left err -> putStrLn "Error"
        Right cfg -> connect $ pack $ botDiscordToken cfg
