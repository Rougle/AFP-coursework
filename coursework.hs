import System.Environment   
import System.Directory  
import System.IO
import System.IO.Unsafe
import Data.List as List
import Data.Map as Map
import Control.Concurrent

inputFileName = "input.txt"
outputFileName = "output.txt"
 
main = do  
    contents <- System.IO.readFile inputFileName  
    let lines = List.lines contents
        g = read (lines !! 0) :: Int
        k = read (lines !! 1) :: Int
        fileNames = drop 2 lines

    children <- newMVar ([])      
    lineCount <- newMVar (0 :: Int) 

    threads <- mapM (\fName -> makeThread (getResultsForFile fName g children lineCount)) fileNames
    _ <- mapM_ takeMVar threads

    results <- takeMVar children
    totalNumberOfLines <- takeMVar lineCount

    writeOutput $ formatResults totalNumberOfLines $ take k $ reverse . sortBy (\(_,a) (_,b) -> compare a b) . Map.toList $ foldListOfMaps results

    
writeOutput :: String -> IO ()
writeOutput content = appendFile outputFileName content
    
--Creates a thread which completion can be monitored with the mvar
makeThread :: IO a -> IO (MVar ())
makeThread proc = do
    handle <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar handle ())
    return handle

--Turns result into pretty print
formatResults :: Int -> [(String, Int)] -> String
formatResults totalNumberOfLines results = List.foldl (\acc x -> acc ++ x) "" $ List.map (\(a:b:_, count) -> a : ' ' : b : ' ' : show count ++ ' ' : show totalNumberOfLines ++ "\n") results

--Gets results for single file
getResultsForFile :: FilePath -> Int -> MVar [Map String Int] -> MVar Int -> IO ()
getResultsForFile fileName g children lineCount = do 
    contents <- System.IO.readFile fileName
    let contentsLined = List.lines contents 
        results = getResultsForLines contentsLined g
        numberOfLines = length contentsLined

    list <- takeMVar children
    putMVar children (list ++ [results])
    totalNumberOfLines <- takeMVar lineCount
    putMVar lineCount (totalNumberOfLines + numberOfLines)

--Combine multiple lines of results
getResultsForLines :: [String] -> Int -> Map String Int
getResultsForLines lines g = foldListOfMaps $ lookupPairsFromLines lines g


getResultsForLines' lines g = List.map (\line -> getAllPairsForLine' g line) lines

    -- Take char
    -- Make pair with gap
        -- Minus gap length from total length, otherwise will fail
    -- Put into list
    -- Group list
    -- AT MOST G CHARS BETWEEN



getAllPairsForLine' 0 _ = []
getAllPairsForLine' g [] = []
getAllPairsForLine' g (x:[]) = []
getAllPairsForLine' g (x:xs) = [(x,y) | y <- take (g + 1) xs] ++ getAllPairsForLine g xs


getCounts xs =  List.map (\group -> (head group, length group)) $ groupTuples xs


groupTuples xs = List.groupBy (\a b -> a == b) xs


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

foldListOfMaps :: [Map String Int] -> Map String Int
foldListOfMaps xs = List.foldl (\acc x -> Map.unionWith (+) acc x) Map.empty xs

lookupPairsFromLines :: [String] -> Int -> [Map String Int]
lookupPairsFromLines lines g = List.map (\line -> Map.fromList $ lookupPairs line g) lines
