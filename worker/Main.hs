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
  eval "window = self" -- HACK because don't know why ghcjs output references window
  self <- jsg "self"
  _ <- self # "postMessage" $ [jsNull]
  buf <- liftIO $ newTVarIO []
  _ <- async $ do
    s <- runXHR' getPeople (ClientEnv (BaseUrl Http "localhost" 8081 ""))
         >>= liftIO . streamSource
    liftIO . flip Streamly.mapM_ s $ \row -> atomically $ do
      b <- readTVar buf
      writeTVar buf (row : b)
  (self <# "onmessage") =<< toJSVal (fun $ \_ _ _ -> do
    rows <- fmap reverse . liftIO . atomically $ do
      b <- readTVar buf
      writeTVar buf []
      return b
    (self # "postMessage") . (:[]) =<< toJSVal rows
    return ())
  rows <- liftIO . atomically $ do
    b <- readTVar buf
    if Prelude.null b then retry else writeTVar buf [] >> return b
  _ <- (self # "postMessage") . (:[]) =<< toJSVal rows
  return ()
