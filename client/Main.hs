{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}


module Main where


import Prelude hiding (div, span)

import Control.Arrow (first)
import Data.CountryCodes
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text hiding (span)
import Language.Javascript.JSaddle
import Shpadoinkle hiding (name)
import Shpadoinkle.Backend.ParDiff
import Shpadoinkle.Html hiding (head, max)
import Shpadoinkle.Widgets.Table
import Shpadoinkle.Widgets.Table.Lazy
import Shpadoinkle.Widgets.Types

import Types


data TableFilters = TableFilters
                  { bySex :: Maybe Sex
                  , byOrigin :: Set CountryCode }
  deriving (Eq, Show)


data FilteredTable = FilteredTable
                   { contents :: [Person]
                   , filters  :: TableFilters }
  deriving (Eq, Show)


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
          countryFilter = if S.null byOrigin then True else S.member (origin p) byOrigin

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
             , checked $ S.member cc (byOrigin (filters tab))
             , onClickE (pur (toggleOriginFilter cc)) ],
      text cName ]

    toggleOriginFilter :: CountryCode -> Model -> Model
    toggleOriginFilter cc (tab, sc) =
      if S.member cc (byOrigin (filters tab))
      then ( tab { filters = (filters tab) { byOrigin = S.delete cc (byOrigin (filters tab)) } }
           , sc )
      else ( tab { filters = (filters tab) { byOrigin = S.insert cc (byOrigin (filters tab)) } }
           , sc )


mainView :: MonadJSM m => DebounceScroll m (LazyTable FilteredTable, SortCol (LazyTable FilteredTable))
         -> (Model, CurrentScrollY) -> HtmlM m (Model, CurrentScrollY)
mainView debounceScroll (m@(tab, sc), sy) = div_ [
    liftMC (first . const) fst $ filterView m,
    lazyTable theme (AssumedTableHeight 500) (AssumedRowHeight 20) (TbodyIsScrollable debounceScroll) id tab sc sy
  ]
  where
    theme :: Theme m FilteredTable
    theme = mempty { bodyProps = const $ const [("style", "display: block; overflow: auto; height: 500px;")]
                   , headProps = const $ const [("style", "display: block;")] }

    container :: Monad m => HtmlM m a -> HtmlM m a
    container = div [("style", "max-height: 500px")] . (:[])


main :: IO ()
main = do
  ts <- debounceRaw 0.25
  let init = ((FilteredTable [] (TableFilters Nothing S.empty), SortCol Name ASC), CurrentScrollY 0)
  model <- newTVarIO init
  runJSorWarp 8080 $
    shpadoinkle Proxy id runParDiff init model (mainView ts) getBody
