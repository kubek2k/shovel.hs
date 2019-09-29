{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text
import qualified Data.Text.IO as TextIO
import Network.AWS
import Network.AWS.SQS
import Options.Applicative
import System.IO
import Text.Regex

data Configuration = Configuration
  { fromURL :: Text
  , toURL :: Text
  }

receiveTenMessages :: Env -> Text -> IO [Message]
receiveTenMessages env url = do
  resp <-
    runResourceT $
    runAWS env $
    within Ireland $ send $ receiveMessage url & rmMaxNumberOfMessages ?~ 10
  return $ resp ^. rmrsMessages

sndMessage :: Env -> Text -> Message -> IO (Maybe SendMessageResponse)
sndMessage env url message = do
  let messageAttributes = message ^. mAttributes
  let messageBody = message ^. mBody
  forM messageBody $ \body ->
    runResourceT $ runAWS env $ within Ireland $ send $ sendMessage url body

delMessage :: Env -> Text -> Text -> IO ()
delMessage env url receiptHandle = do
  runResourceT $
    runAWS env $ within Ireland $ send $ deleteMessage url receiptHandle
  return ()

getMessageId :: Message -> Text
getMessageId message = fromMaybe "" $ message ^. mMessageId

shovelNextTen :: Env -> Text -> Text -> IO Bool
shovelNextTen env fromURL toURL = do
  putStrLn "Shoveling next ten messages"
  messages <- receiveTenMessages env fromURL
  putStrLn $ "Got " <> show (Prelude.length messages) <> " messages to shovel"
  forM_ messages $ \message -> do
    TextIO.putStrLn $
      "Sending message " <> getMessageId message <> " to the queue " <> toURL
    sndMessage env toURL message
    forM (message ^. mReceiptHandle) $ \handle -> delMessage env fromURL handle
  return $ Prelude.length messages > 0

shovelAll :: Env -> Text -> Text -> IO ()
shovelAll env fromURL toURL = do
  processedMessages <- shovelNextTen env fromURL toURL
  when processedMessages $ shovelAll env fromURL toURL

configurationParser :: Parser Configuration
configurationParser =
  Configuration <$>
  Options.Applicative.argument
    str
    (metavar "FROM_URL" <> help "Url of queue to get messages from") <*>
  Options.Applicative.argument
    str
    (metavar "TO_URL" <> help "Url of queue to put messages to")

sqsURLRegex = mkRegex "https://sqs.[^.]+.amazonaws.com/[0-9]+/[^/]+$"

readSQSURL :: String -> Either String String
readSQSURL s =
  case matchRegex sqsURLRegex of
    Nothing ->
      Left $ "Url " <> s <> " doesn't match regex " <> print sqsURLRegex
    Just u -> Right u

parseCommandLine :: IO Configuration
parseCommandLine =
  execParser $
  info
    (configurationParser <**> helper)
    (fullDesc <> progDesc "Move messages from one queue to other" <>
     header "shovel.hs - SQS messages shovel")

main :: IO ()
main = do
  configuration <- parseCommandLine
  env <- newEnv Discover
  shovelAll env (fromURL configuration) (toURL configuration)
  putStrLn "No more messages to shovel"
