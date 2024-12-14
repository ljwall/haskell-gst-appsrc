{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int
import Control.Monad
import GI.Gst as Gst
import GI.GObject as GObj
import System.Posix.Signals

-- | Loop forever for messages
loop :: Gst.Bus -> IO ()
loop bus = do
  maybeMsg <- Gst.busTimedPopFiltered bus 1000000000 [Gst.MessageTypeError, Gst.MessageTypeEos]
  case maybeMsg of
    Nothing -> do putStrLn "Tick.."
                  loop bus
    Just _  -> putStrLn "got an error or eos"


-- | Send EOS to the (pipeline) element
sendEos ::  Gst.Pipeline -> IO Bool
sendEos pipeline = Gst.eventNewEos >>= Gst.elementSendEvent pipeline


unwrap :: (MonadFail m)
       => m (Maybe a)
       -> String
       -> m a
unwrap m err = do
  val <- m
  case val of
    Nothing        -> fail err
    Just something -> pure something


main :: IO ()
main = do
  -- Initialise Gstreamer
  _ <- Gst.init Nothing

  -- Create and play pipeline
  pipeline <- Gst.pipelineNew Nothing
  videotestsrc <- Gst.elementFactoryMake "videotestsrc" Nothing `unwrap` "Unable to make element"
  videoconvert <- Gst.elementFactoryMake "videoconvert" Nothing `unwrap` "Unable to make element"
  autovideosink <- Gst.elementFactoryMake "autovideosink" Nothing `unwrap` "Unable to make element"

  pattern <- toGValue (1 :: Int32)
  GObj.objectSetProperty videotestsrc "pattern" pattern

  _ <- Gst.binAdd pipeline videotestsrc
  _ <- Gst.binAdd pipeline videoconvert
  _ <- Gst.binAdd pipeline autovideosink

  _ <- Gst.elementLink videotestsrc videoconvert
  _ <- Gst.elementLink videoconvert autovideosink

  _ <- Gst.elementSetState  pipeline Gst.StatePlaying

  -- Install a signal handler that will send an EOS to the pipeline on sigint
  _ <- installHandler sigINT (CatchOnce . void $ sendEos pipeline) Nothing

  -- Get the message bus and start looping
  bus <- Gst.elementGetBus pipeline `unwrap` "No bus :-( "
  putStrLn "Waiting for eos or error"
  loop bus
