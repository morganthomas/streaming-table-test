{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}

module Main where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Function
import Data.Proxy
import Data.Text hiding (reverse)
import GHC.Conc
import Language.Javascript.JSaddle
import Servant.Types.SourceT
import Shpadoinkle.Router.Client
import Streamly hiding (async)
import qualified Streamly.Prelude as Streamly

import Types

default (Text)

getPeople :: ClientM (SourceT IO Person)
getPeople = client (Proxy @Api)

streamSource :: SourceT IO a -> IO (SerialT IO a)
streamSource src = do
  push <- newEmptyMVar
  _ <- forkIO . unSourceT src . fix $ \go -> \case
    Stop -> return ()
    Error _ -> return ()
    Skip rest -> go rest
    Yield x rest -> do
      putMVar push x
      go rest
    Effect m -> do
      rest <- m
      go rest
  return $ Streamly.repeatM (takeMVar push)

#ifdef ghcjs_HOST_OS
ioJSM = id
#else
ioJSM _ = putStrLn "worker only runs in ghcjs"
#endif

main :: IO ()
main = ioJSM $ do
  eval "console.log('in worker thread')"
  eval "window = self" -- HACK because don't know why ghcjs output references window
  self <- jsg "self"
  buf <- liftIO $ newTVarIO []
  _ <- async $ do
    eval "console.log('in buffering thread')"
    s <- runXHR' getPeople (ClientEnv (BaseUrl Http "localhost" 8081 ""))
         >>= liftIO . streamSource
    eval "console.log('beginning to buffer')"
    liftIO . flip Streamly.mapM_ s $ \row -> atomically $ do
      b <- readTVar buf
      writeTVar buf (row : b)
  (self <# "onmessage") =<< toJSVal (fun $ \_ _ _ -> do
    eval "console.log('in onmessage handler')"
    rows <- fmap reverse . liftIO . atomically $ do
      b <- readTVar buf
      writeTVar buf []
      return b
    eval "console.log('emptied buffer')"
    (self # "postMessage") . (:[]) =<< toJSVal rows
    eval "console.log('posted response')"
    return ())
  rows <- liftIO . atomically $ do
    b <- readTVar buf
    if Prelude.null b then retry else writeTVar buf [] >> return b
  _ <- (self # "postMessage") . (:[]) =<< toJSVal rows
  return ()
