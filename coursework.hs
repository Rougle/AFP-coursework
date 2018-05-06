import System.Environment   
import System.Directory  
import System.IO
import System.IO.Unsafe
import Data.List as List
import Data.Map as Map

import Control.Concurrent

--main = do  
   --readF "input.txt"

-- Line 1: gap constraint
-- Line 2: top x pairs
-- Line 3-y: The files containing data

-- CONCURRENCY

children :: MVar [MVar (Map String Int)]
children = System.IO.Unsafe.unsafePerformIO (newMVar [])

waitForChildren :: IO (Map String Int)
waitForChildren = do
    listOfResults <- takeMVar children
    unpackedResults <- mapM takeMVar listOfResults
    return (foldListOfMaps unpackedResults)

    
forkChild :: Map String Int -> IO ()
forkChild result = do
    mvar <- newMVar result
    childs <- takeMVar children
    putMVar children (mvar:childs)
    --forkFinally (\_ -> putMVar mvar Map.empty)

--



readF :: String -> IO ()  
readF fileName = do  
    contents <- System.IO.readFile fileName  
    let lines = List.lines contents
        g = read (lines !! 0) :: Int
        k = read (lines !! 1) :: Int
        fileNames = drop 2 lines
        testFile = head fileNames
        x = List.map (\fileName -> getResultsForFile fileName g) fileNames
        
    results <- waitForChildren


    --testResult <- getResultsForFile testFile g
        
    --System.IO.putStr $ List.unlines fileNames
    --System.IO.putStr $ showTree testResult
    System.IO.putStr "Done.\n"
    
    System.IO.putStr $ show (length results)

    System.IO.putStr "\n"


getResultsForFile :: FilePath -> Int -> IO ()
getResultsForFile fileName g = do 
    contents <- System.IO.readFile fileName
    let contentsLined = List.lines contents 
        results = getResultsForLines contentsLined g
    forkChild results
    return ()

--Combine multiple lines of results
getResultsForLines :: [String] -> Int -> Map String Int
getResultsForLines lines g = foldListOfMaps $ lookupPairsFromLines lines g

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
