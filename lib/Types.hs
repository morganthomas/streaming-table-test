module Types where


import Data.CountryCodes
import Data.Text
import Test.QuickCheck

import StockName


data Sex = Male | Female
  deriving (Eq, Ord, Show)


data Person = Person
            { name   :: Text
            , age    :: Int
            , sex    :: Sex
            , origin :: CountryCode }
  deriving (Eq, Show)


instance Arbitrary Sex where
  arbitrary = oneof [return Male, return Female]


instance Arbitrary CountryCode where
  arbitrary = elements (fst <$> allNames)


instance Arbitrary Person where
  arbitrary = Person <$> (unStockName <$> arbitrary) <*> choose (0,120) <*> arbitrary <*> arbitrary
