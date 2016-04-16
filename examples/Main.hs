module Main where
import Data.Map.Extensions as Map
import Data.Map.Function
import Data.Function

type Name = String
type Job = String
type Age = Int

fixture :: [(Name, Job, Age)]
fixture = [ ("Alice", "Engineer", 25)
          , ("Bob"  , "Engineer", 35)
          , ("Jane" , "Engineer", 28)
          , ("Ryan" , "Designer", 50)
          , ("Zoe"  , "Artist"  , 21)
          ]

average :: (Integral v, Fractional s) => Map k v -> s
average xs = Map.foldr (+) 0 (fromIntegral <$> xs) / fromIntegral (Map.size xs)

main :: IO ()
main = do
  print $ fromList2 fixture 
  putStrLn ""

  print $ transpose $ fromList2 fixture
  putStrLn ""

  -- print the average age in a job
  fromList2 fixture
    & transpose
    <&> average
    <&> round
    & print
  putStrLn ""

  -- Find all occupations grouped by age
  fromList2 fixture
    <&> groupElemsBy (\age -> show (age `div` 10 * 10) ++ "'s")
    & transpose
    <&> Map.unions . Map.elems
    & print
  putStrLn ""

