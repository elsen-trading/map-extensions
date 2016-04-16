-- Example using the Titanic data from Kaggle
-- https://www.kaggle.com/c/titanic

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Csv
import           Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import           Data.Foldable (toList)
import           Data.Map.Extensions as Map hiding (toList)
import qualified Data.Map.Extensions as Map
import           Data.Map.Function

data Gender
  = Male
  | Female
    deriving (Eq, Ord, Show)

data EmbarkPort
  = Cherbourg
  | Queenstown
  | Southampton
    deriving (Eq, Ord, Show)

data Passenger = Passenger
  { id       :: Int
  , survived :: Bool
  , pclass   :: Int
  , name     :: String
  , sex      :: Gender
  , age      :: Maybe Double
  , sibsp    :: Int
  , parch    :: Int
  , ticket   :: String
  , fare     :: Double
  , cabin    :: String
  , embarked :: Maybe EmbarkPort
  }
    deriving Show

instance FromField Gender where
  parseField s = case s of
    "male"   -> pure Male
    "female" -> pure Female
    _        -> fail . B.unpack $ "Could not parse gender '" <> s <> "'"

instance FromField EmbarkPort where
  parseField s = case s of
    "Q" -> pure Queenstown
    "C" -> pure Cherbourg
    "S" -> pure Southampton
    _   -> fail . B.unpack $ "Could not parse embarked '" <> s <> "'"

instance FromNamedRecord Passenger where
  parseNamedRecord r = Passenger
    <$> r.:"PassengerId"
    <*> ((==one) <$> r.:"Survived")
    <*> (toEnum  <$> r.:"Pclass")
    <*> r.:"Name"
    <*> r.:"Sex"
    <*> r.:"Age"
    <*> r.:"SibSp"
    <*> r.:"Parch"
    <*> r.:"Ticket"
    <*> r.:"Fare"
    <*> r.:"Cabin"
    <*> r.:"Embarked"
    where
      one = 1 :: Int

decodeFile :: FilePath -> IO (Either String [Passenger])
decodeFile path = fmap (toList . snd) . decodeByName <$> BL.readFile path

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

fmap3 :: (Functor f, Functor g, Functor h) =>
  (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

count :: (Foldable t, Integral int) => t a -> int
count = Prelude.foldr (\_ b -> b + 1) 0

mean :: (Foldable t, Fractional a) => t a -> a
mean xs = sum xs / fromIntegral (count xs)

switch :: a -> a -> Bool -> a
switch x y cond = if cond then x else y

-- round decimal places
roundPlaces :: Int -> Double -> Double
roundPlaces places f = fromIntegral (round (coeff * f)) / coeff
  where
    coeff = 10.0**(fromIntegral places)

fmtPct :: Double -> String
fmtPct d = show (roundPlaces 2 (d*100)) <> "%"

main = do
  passengers <- decodeFile "titanic.csv"
  case passengers of
    Left  s -> putStrLn s
    Right ps -> do
      print $ groupBy sex ps
        <&> groupBy pclass
        <&&> length

      -- probability of survivorship
      print $ groupBy sex ps
        <&> groupBy pclass
        <&&&> switch 1 0 . survived
        <&&> fmtPct . mean

