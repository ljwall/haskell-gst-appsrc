{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Word
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import GI.Gst as Gst
import GI.GObject as GObj
import System.Posix.Signals

import qualified Data.ByteString as B

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

greyness :: Word64 -> B.ByteString
greyness t =
  let v = floor $ 255.0 * (fromIntegral t) / 10000000000.0
  in B.replicate (4*240*360) v

supplyBuffers :: Gst.Pipeline -> Gst.Element -> Word64 -> IO ()
supplyBuffers pipeline appsrc t = do
  buff <- bufferNewWrapped $ greyness t

  -- (hasPos, pos) <- elementQueryPosition pipeline FormatTime
  -- let t = if hasPos then pos else 0

  setBufferPts buff (fromIntegral t)

  typ <- typeFromName "GstAppSrc"
  sig <- signalLookup "push-buffer" typ
  gAppsrc <- toGValue $ Just appsrc
  gBuff <- toGValue $ Just buff
  _ <- signalEmitv [gAppsrc, gBuff] sig 0
  threadDelay 100000
  supplyBuffers pipeline appsrc (t + 100000000)

main :: IO ()
main = do
  -- Initialise Gstreamer
  _ <- Gst.init Nothing

  -- Create and play pipeline
  pipeline <- Gst.pipelineNew Nothing
  -- videotestsrc <- Gst.elementFactoryMake "videotestsrc" Nothing `unwrap` "Unable to make element"
  appsrc <- Gst.elementFactoryMake "appsrc" Nothing `unwrap` "Unable to make element"
  videoconvert <- Gst.elementFactoryMake "videoconvert" Nothing `unwrap` "Unable to make element"
  autovideosink <- Gst.elementFactoryMake "autovideosink" Nothing `unwrap` "Unable to make element"

  caps <- capsFromString "video/x-raw,format=RGBx,width=360,height=240,framerate=0/1" `unwrap` "Unable to make caps"
  gCaps <- toGValue $ Just caps
  GObj.objectSetProperty appsrc "caps" gCaps
  gFalse <- toGValue False
  GObj.objectSetProperty appsrc "emit-signals" gFalse
  gTrue <- toGValue True
  GObj.objectSetProperty appsrc "is-live" gTrue

  _ <- Gst.binAdd pipeline appsrc
  _ <- Gst.binAdd pipeline videoconvert
  _ <- Gst.binAdd pipeline autovideosink

  _ <- Gst.elementLink appsrc videoconvert
  _ <- Gst.elementLink videoconvert autovideosink

  _ <- Gst.elementSetState  pipeline Gst.StatePlaying

  -- Install a signal handler that will send an EOS to the pipeline on sigint
  _ <- installHandler sigINT (CatchOnce . void $ sendEos pipeline) Nothing

  -- Get the message bus and start looping
  bus <- Gst.elementGetBus pipeline `unwrap` "No bus :-( "
  putStrLn "Waiting for eos or error"
  concurrently_  (loop bus) $ supplyBuffers pipeline appsrc 0
