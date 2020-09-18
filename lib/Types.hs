{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Types where


import Data.Aeson
import Data.CountryCodes
import Data.Text
import Servant
import Test.QuickCheck

import StockName


data Sex = Male | Female
  deriving (Eq, Ord, Show)

instance ToJSON Sex where
  toJSON Male = toJSON @Text "male"
  toJSON Female = toJSON @Text "female"

instance FromJSON Sex where
  parseJSON (String "male") = return Male
  parseJSON (String "female") = return Female
  parseJSON _ = fail "not a valid sex"


data Person = Person
            { name   :: Text
            , age    :: Int
            , sex    :: Sex
            , origin :: CountryCode }
  deriving (Eq, Show)

instance ToJSON Person where
  toJSON p = object [ "name" .= name p
                    , "age" .= age p
                    , "sex" .= sex p
                    , "origin" .= origin p ]

instance FromJSON Person where
  parseJSON = withObject "Person" $ \p -> do
    n <- p .: "name"
    a <- p .: "age"
    s <- p .: "sex"
    o <- p .: "origin"
    return (Person n a s o)


instance Arbitrary Sex where
  arbitrary = oneof [return Male, return Female]


instance Arbitrary CountryCode where
  arbitrary = elements (fst <$> allNames)


instance Arbitrary Person where
  arbitrary = Person <$> (unStockName <$> arbitrary) <*> choose (0,120) <*> arbitrary <*> arbitrary


type Api = StreamGet NewlineFraming JSON (SourceIO Person)
