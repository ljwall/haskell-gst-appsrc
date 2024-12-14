{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Word
import Data.UnixTime
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Foreign.C.Types (CTime(..))
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
  let v = floor $ 255.0 * (fromIntegral t :: Double) / 100.0
  in B.replicate (4*240*360) v

supplyBuffers :: UnixTime -> Gst.Pipeline -> Gst.Element -> Word64 -> IO ()
supplyBuffers start pipeline appsrc n = do
  buff <- bufferNewWrapped $ greyness n

  let step_us = 33000

  now <- getUnixTime
  let UnixDiffTime{udtSeconds=(CTime s), udtMicroSeconds=us} = diffUnixTime now start
      t = (fromIntegral s * 1000000000 + fromIntegral us * 1000) :: Int

  setBufferPts buff (fromIntegral t)

  typ <- typeFromName "GstAppSrc"
  sig <- signalLookup "push-buffer" typ
  gAppsrc <- toGValue $ Just appsrc
  gBuff <- toGValue $ Just buff
  _ <- signalEmitv [gAppsrc, gBuff] sig 0
  threadDelay step_us
  supplyBuffers start pipeline appsrc (n + 1)

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

  -- x264enc <- Gst.elementFactoryMake "x264enc" Nothing `unwrap` "Unable to make element"
  -- h264parse <- Gst.elementFactoryMake "h264parse" Nothing `unwrap` "Unable to make element"
  -- mp4mux <- Gst.elementFactoryMake "mp4mux" Nothing `unwrap` "Unable to make element"
  -- filesink <- Gst.elementFactoryMake "filesink" Nothing `unwrap` "Unable to make element"

  caps <- capsFromString "video/x-raw,format=RGBx,width=360,height=240,framerate=0/1" `unwrap` "Unable to make caps"
  gCaps <- toGValue $ Just caps
  GObj.objectSetProperty appsrc "caps" gCaps
  gFalse <- toGValue False
  GObj.objectSetProperty appsrc "emit-signals" gFalse

  gTrue <- toGValue True
  GObj.objectSetProperty appsrc "is-live" gTrue

  -- gLoc <- toGValue $ Just ("out.mp4" :: String)
  -- GObj.objectSetProperty filesink "location" gLoc

  _ <- Gst.binAdd pipeline appsrc
  _ <- Gst.binAdd pipeline videoconvert
  _ <- Gst.binAdd pipeline autovideosink
  -- _ <- Gst.binAdd pipeline x264enc
  -- _ <- Gst.binAdd pipeline h264parse
  -- _ <- Gst.binAdd pipeline mp4mux
  -- _ <- Gst.binAdd pipeline filesink

  _ <- Gst.elementLink appsrc videoconvert
  _ <- Gst.elementLink videoconvert autovideosink
  -- _ <- Gst.elementLink videoconvert x264enc
  -- _ <- Gst.elementLink x264enc h264parse
  -- _ <- Gst.elementLink h264parse mp4mux
  -- _ <- Gst.elementLink mp4mux filesink

  _ <- Gst.elementSetState  pipeline Gst.StatePlaying

  -- Install a signal handler that will send an EOS to the pipeline on sigint
  _ <- installHandler sigINT (CatchOnce . void $ sendEos pipeline) Nothing

  -- Get the message bus and start looping
  bus <- Gst.elementGetBus pipeline `unwrap` "No bus :-( "
  start <- getUnixTime
  putStrLn "Waiting for eos or error"
  concurrently_  (loop bus) $ supplyBuffers start pipeline appsrc 0
