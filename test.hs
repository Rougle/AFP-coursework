import System.Environment   
import System.Directory  
import System.IO
import System.IO.Unsafe
import Data.List as List
import Data.Map as Map

import Control.Concurrent

getResultsForLines' :: [String] -> Int -> [[(String, Int)]]
getResultsForLines' lines g = List.map (\line -> frequency $ getResultsForLine' g line) lines

getResultsForLine' :: Int -> String -> [String]
getResultsForLine' 0 _ = []
getResultsForLine' g [] = []
getResultsForLine' g (x:[]) = []
getResultsForLine' g (x:xs) = [ x:y:[] | y <- take (g + 1) xs] ++ getResultsForLine' g xs


frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

--getCounts :: [[String]] -> [(String, Int)]
--getCounts xs = List.map (\group -> (head group, length group)) xs

--groupTuples :: [String] -> [[String]]
--groupTuples xs = List.groupBy (\(a, b) (i, j) -> a == i && b == j) xs



--getAllPairsForChar' :: Int -> String -> [(Char, Char)]
--getAllPairsForChar' g xs = [(x,y) | x <- [head xs], y <- take g xs]

