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

    writeOutput $ formatResults totalNumberOfLines $ take k $ reverse $ sortBy (\(_,a) (_,b) -> compare a b) $ combineResults results

    
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
getResultsForFile :: FilePath -> Int -> MVar [(String, Int)] -> MVar Int -> IO ()
getResultsForFile fileName g children lineCount = do 
    contents <- System.IO.readFile fileName
    let contentsLined = List.lines contents 
        results = getResultsForLines contentsLined g
        numberOfLines = length contentsLined

    list <- takeMVar children
    putMVar children (list ++ results)
    totalNumberOfLines <- takeMVar lineCount
    putMVar lineCount (totalNumberOfLines + numberOfLines)

getResultsForLines :: [String] -> Int -> [(String, Int)]
getResultsForLines lines g = frequency $ List.foldl (\acc x -> acc ++ x) [] $ List.map (\line -> getResultsForLine g line) lines

getResultsForLine :: Int -> String -> [String]
getResultsForLine 0 _ = []
getResultsForLine g [] = []
getResultsForLine g (x:[]) = []
getResultsForLine g (x:xs) = [ x:y:[] | y <- take (g + 1) xs] ++ getResultsForLine g xs

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

combineResults :: (Ord a) => [(a, Int)] -> [(a, Int)]
combineResults xs = toList (fromListWith (+) xs)