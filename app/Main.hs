{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import GI.Gst as Gst
import System.Exit
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
sendEos :: Gst.Element -> IO Bool
sendEos pipeline = Gst.eventNewEos >>= Gst.elementSendEvent pipeline


main :: IO ()
main = do
  -- Initialise Gstreamer
  _ <- Gst.init Nothing

  -- let pipeTxt = "videotestsrc is-live=true ! x264enc key-int-max=25 ! h264parse ! splitmuxsink location=vid%03d.mp4 max-size-time=5000000000"
  let pipeTxt = "videotestsrc is-live=true ! videoconvert ! autovideosink"

  -- Create and play pipeline
  pipeline <- Gst.parseLaunch pipeTxt
  _ <- Gst.elementSetState  pipeline Gst.StatePlaying

  -- Install a signal handler that will send an EOS to the pipeline on sigint
  _ <- installHandler sigINT (CatchOnce . void $ sendEos pipeline) Nothing

  -- Get the message bus and start looping
  maybeBus <- Gst.elementGetBus pipeline
  case maybeBus of
    Nothing  ->
      do _ <- Gst.elementSetState  pipeline Gst.StateNull
         die "No bus! :-( "
    Just bus ->
      do putStrLn "Waiting for eos or error"
         loop bus
 
