{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}


module Main where


import Prelude hiding (div, span)

import Control.Arrow (first)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.CountryCodes
import Data.Function (fix)
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text hiding (span, reverse)
import GHC.Conc
import Language.Javascript.JSaddle
import Servant.Types.SourceT
import Shpadoinkle hiding (name, newTVarIO)
import Shpadoinkle.Backend.ParDiff
import Shpadoinkle.Html hiding (head, max)
import Shpadoinkle.Router.Client (ClientM, ClientEnv (..), client, runXHR', BaseUrl (..), Scheme (Http))
import Shpadoinkle.Widgets.Table
import Shpadoinkle.Widgets.Table.Lazy
import Shpadoinkle.Widgets.Types
import Streamly hiding (async)
import qualified Streamly.Prelude as Streamly
import qualified Streamly.Internal.Data.Fold.Types as Streamly

import Types

default (Text)


data TableFilters = TableFilters
                  { bySex :: Maybe Sex
                  , byOrigin :: Set CountryCode }
  deriving (Eq, Show)


data FilteredTable = FilteredTable
                   { contents :: [Person]
                   , filters  :: TableFilters }
  deriving (Show)

instance Eq FilteredTable where
  _ == _ = False

instance LazyTabular FilteredTable where
  countRows _ = 10000


data instance Row FilteredTable = PersonRow { unRow :: Person }


data instance Column FilteredTable = Name | Age | Sex | Origin
  deriving (Eq, Ord, Show, Bounded, Enum)


instance Humanize (Column FilteredTable) where
  humanize = humanize . show


instance Tabular FilteredTable where
  type Effect FilteredTable m = Monad m

  toRows xs = PersonRow <$> contents xs

  toFilter (filters -> TableFilters {..}) (unRow -> p) = sexFilter && countryFilter
    where sexFilter = case bySex of
            Just s  -> sex p == s
            Nothing -> True
          countryFilter = if Set.null byOrigin then True else Set.member (origin p) byOrigin

  sortTable (SortCol c s) (unRow -> a) (unRow -> b) =
    case c of
      Name   -> compareOn s (name a)   (name b)
      Age    -> compareOn s (age a)    (age b)
      Sex    -> compareOn s (sex a)    (sex b)
      Origin -> compareOn s (origin a) (origin b)

  toCell tab (unRow -> p) Name   = [text (name p)]
  toCell tab (unRow -> p) Age    = [text (pack (show (age p)))]
  toCell tab (unRow -> p) Sex    = [text (pack (show (sex p)))]
  toCell tab (unRow -> p) Origin = [text (toName (origin p))]


type Model = (FilteredTable, SortCol FilteredTable)


filterView :: Monad m => Model -> HtmlM m Model
filterView m =
  div_ [
    div_ [
      text "Filter by sex: ",
      span [ onClickE (pur (setSexFilter (Just Male))) ] [ "Male" ],
      text " / ",
      span [ onClickE (pur (setSexFilter (Just Female))) ] [ "Female" ],
      text " / ",
      span [ onClickE (pur (setSexFilter Nothing)) ] [ "Either" ]
    ],
    div_ [
      text "Filter by country of origin:",
      div_ $ originWidget m <$> allNames
    ]
  ]

  where
    setSexFilter :: Maybe Sex -> Model -> Model
    setSexFilter f (tab, sc) = (tab { filters = (filters tab) { bySex = f } }, sc)

    originWidget :: Monad m => Model -> (CountryCode, Text) -> HtmlM m Model
    originWidget (tab, sc) (cc, cName) = div_ [
      input' [ ("type", "checkbox")
             , checked $ Set.member cc (byOrigin (filters tab))
             , onClickE (pur (toggleOriginFilter cc)) ],
      text cName ]

    toggleOriginFilter :: CountryCode -> Model -> Model
    toggleOriginFilter cc (tab, sc) =
      if Set.member cc (byOrigin (filters tab))
      then ( tab { filters = (filters tab) { byOrigin = Set.delete cc (byOrigin (filters tab)) } }
           , sc )
      else ( tab { filters = (filters tab) { byOrigin = Set.insert cc (byOrigin (filters tab)) } }
           , sc )


mainView :: MonadJSM m => DebounceScroll m (LazyTable FilteredTable, SortCol (LazyTable FilteredTable))
         -> (Model, CurrentScrollY) -> HtmlM m (Model, CurrentScrollY)
mainView debounceScroll (m@(tab, sc), sy) = div_ [
    lazyTable theme (AssumedTableHeight 500) (AssumedRowHeight 20) (TbodyIsScrollable debounceScroll) id tab sc sy,
    liftMC (first . const) fst $ filterView m
  ]
  where
    theme :: Theme m FilteredTable
    theme = mempty { bodyProps = const $ const [("style", "display: block; overflow: auto; height: 500px;")]
                   , headProps = const $ const [("style", "display: block;")] }

    container :: Monad m => HtmlM m a -> HtmlM m a
    container = div [("style", "max-height: 500px")] . (:[])


main :: IO ()
main = do
  ds <- debounceRaw 0.25
  let init = ((FilteredTable [] (TableFilters Nothing Set.empty), SortCol Name ASC), CurrentScrollY 0)
  model <- newTVarIO init
  runJSorWarp 8080 $ do
    win <- jsg "window"
    ready <- liftIO $ newTVarIO False
    eval "console.log('created ready tvar')"
    worker <- eval "new Worker('http://localhost:8082/bin/worker.jsexe/all.js');"
    eval "console.log('created worker')"
    (worker <# "onmessage") =<< toJSVal (fun (\_ _ -> \case
      [val] -> do
        eval "console.log('received worker message')"
        console <- jsg "console"
        console # "log" $ [val]
        people <- fromMaybe [] <$> (fromJSVal =<< (val ! "data"))
        eval "console.log('extracted people object')"
        liftIO . atomically $ do
          ((ft, sc), sy) <- readTVar model
          writeTVar model ((ft { contents = contents ft ++ people }, sc), sy)
          writeTVar ready True
        eval "console.log('updated state')"
        return ()))
    eval "console.log('attached onmessage handler to worker')"
    _ <- async . forever $ do
      liftIO . atomically $ do
        isReady <- readTVar ready
        if isReady
          then return ()
          else retry
      (win # "requestAnimationFrame") . (:[]) =<< toJSVal (fun (\_ _ _ -> do
        eval "console.log('requesting buffer flush')"
        void $ worker # "postMessage" $ [jsNull]))
      liftIO . atomically $ writeTVar ready False
      return ()
    eval "console.log('created buffer flush event loop')"
    _ <- async $ do
      shpadoinkle Proxy id runParDiff init model (mainView ds) getBody
      eval "console.log('shpadoinkled')"
      return ()
    eval "console.log('kicked off shpadoinkle')"
    return ()
