import System.Environment   
import System.Directory  
import System.IO  
import Data.List
import Data.Text
import Data.Text.IO

main = do  
   readFile "input.txt"

-- Line 1: gap constraint
-- Line 2: top x pairs
-- Line 3-y: The files conaining data

readFile :: [String] -> IO ()  
readFile [fileName] = do  
    contents <- System.IO.readFile fileName  
    let lines = Data.List.lines contents
    --Parse the first two lines
    --Start processes for data files    
    System.IO.putStr $ Data.List.unlines lines  



--Combine multiple lines of results
testF g lines = foldListOfMaps $ lookupPairsFromLines lines g

--Lookup pairs with max gap of g
lookupPairs :: String -> Int -> [(String, Int)]
lookupPairs line g = List.map (\(x,y) -> (x:y:[], lookupPair line (x,y) g)) charPairs 

--Gets count of pairs for a single pair
lookupPair :: [Char] -> (Char, Char) -> Int -> Int
lookupPair inputLine (x,y) g = 
    let xIndeces = findCharIndeces inputLine x
        yIndeces = findCharIndeces inputLine y
    in getPairCount xIndeces yIndeces g

--Generates all possible pairs of chars
charPairs :: [(Char,Char)]
charPairs = [(i,j) | i <- ['a'..'z'], j <- ['a'..'z'] ]

--Turns list xs of chars into list of indexes where char c was found
findCharIndeces :: [Char] -> Char -> [Int]
findCharIndeces xs c = c `elemIndices` xs

--Count pairs from lists of indexes of the chars
getPairCount :: [Int] -> [Int] -> Int -> Int
getPairCount xs ys g = sum $ List.map (getPairCountForIndex ys g) xs 

--Return the amount of pairs index x has in ys with gap g
getPairCountForIndex :: [Int] -> Int -> Int -> Int
getPairCountForIndex ys g x = length $ List.filter (\z -> z <= (x + g + 1) && z > x) ys

filterEmpty :: [(String, Int)] -> [(String, Int)] 
filterEmpty keyValues = List.filter (\(key,z) -> z > 0) keyValues

foldListOfMaps :: [Map String Int] -> Map String Int
foldListOfMaps xs = List.foldl (\acc x -> Map.unionWith (+) acc x) Map.empty xs

lookupPairsFromLines :: [String] -> Int -> [Map String Int]
lookupPairsFromLines lines g = List.map (\line -> Map.fromList $ lookupPairs line g) lines