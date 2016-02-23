module Main where
import Data.Map.Extensions as Map

type Name = String
type Job = String
type Age = Double

fixture :: [(Name, Job, Age)]
fixture = [ ("Alice", "Engineer", 25)
          , ("Bob"  , "Engineer", 35)
          , ("Ryan" , "Designer", 50)
          , ("Zoe"  , "Artist"  , 21)
          ]

average :: (Fractional v) => Map k v -> v
average xs = Map.foldr (+) 0 xs / fromIntegral (Map.size xs)

main :: IO ()
main = do
  print $ fromList2 fixture 
  print $ transpose $ fromList2 fixture
  -- print the average age in a job
  print $ fmap (average) $ transpose $ fromList2 fixture
