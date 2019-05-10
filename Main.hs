{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text
import qualified Data.Text.IO as TextIO
import Network.AWS
import Network.AWS.SQS
import System.IO

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

shovelAll :: Text -> Text -> IO ()
shovelAll fromURL toURL = do
  env <- newEnv Discover
  processedMessages <- shovelNextTen env fromURL toURL
  when processedMessages $ shovelAll fromURL toURL

main :: IO ()
main = do
  env <- newEnv Discover
  shovelAll "queue1" "queue2"
  putStrLn "No more messages to shovel"
