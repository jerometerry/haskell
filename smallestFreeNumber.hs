-- Jerome Terry 
-- April 19, 2013
--
-- Adapted from Pearls of Functional Algorithm Design, Richard Bird, 2010
-- Chapter 1, The smallest free number
-- 
module Main where
import Data.Array
import Data.List

-- Synonym that represents a request to compute the smalltest free number from a list of natural numbers
type SFNReq = (String, [Int], Int)

-- Synonym that represents a result to an SFNReq, which is just the original SFNReq tuple that also includes the smallest free number
type SFNRes = (String, [Int], Int, Int)

-- Generates a list of list of natural numbers, finds the smallest free number in each list, and outputs the results to stdout
main :: IO ()
main = let lists       = generateTestLists
           minFreeNums = computeSmallestFreeNumbers lists
           actions     = printSmallestFreeNumbers minFreeNums
       in  bindActions actions

-- Gets the smallest free number from a list of natural numbers       
-- @param  s The name of the list
-- @param  xs The list of natural numbers, sorted or unsorted
-- @param  e The expected smallest free number
-- @return A SFNRes data type containing the original request, as well as the computed smallest free number
getMinFree :: SFNReq -> SFNRes
getMinFree (s, xs, e) = (s, xs, e, getMinFree' xs 0 $ length xs)

-- Gets the smallest free number from the given list of natural numbers
--
-- For example, given the list [0, 1, 2, 4, 5, 6], the smallest free value is 3.
-- The call stacks is as follows:
--
-- Initial call
-- getMinFree' [0, 1, 2, 4, 5, 6] 0 6
-- start = 0, len = 6, pv = 0+1+(6/2)=4
-- left = [0, 1, 2], right = [4,5,6]
-- leftLen = 3, rightLen = 6 - 3 = 3
-- rightStart = 0 + 3 = 3
-- inRight = 3 == 4 = False
--
-- 1st recursive call
-- getMinFree' [0,1,2] 0 3
-- start = 0, len = 3, pv = 0+1+(3/2) = 2
-- left = [0,1], right = [2]
-- leftLen = 2, rightLen = 3 - 2 = 1
-- rightStart = 0 + 2 = 2
-- inRight = 2 == 2 = True
--
-- 2nd recursive call
-- getMinFree' [2] 2 1
-- start = 2, len = 1, pv = 2+1+(1/2) = 3
-- left = [2], right = []
-- leftLen = 1, rightLen = 1 - 1 = 0
-- rightStart = 2 + 1 = 3
-- inRight = 3 == 3 = True
-- 
-- 3rd recursive call
-- getMinFree' [] 3 0
-- start = 3, len = 0, pv = 3+1+(0/2) = 4
-- since len = 0, smallest free number = start = 3
-- 
-- @param  xs The list of natural numbers, sorted or unsorted
-- @param  start The value associated with index 0 in xs
-- @param  len The number of elements contained in xs
-- @return The smallest free number in the given list of natural numbers
getMinFree' ::  [Int] -> Int -> Int -> Int
getMinFree' xs start len
    -- If xs is empty, then the smallest free number is just the start value
    | len == 0          = start          

    -- The smallest free value exists in the right partition.
    -- Search for the smallest free number >= pv
    | inRight           = getMinFree'  right pv rightLen    
    
    -- This smallest free value exists in the left partition.
    -- Find the smallest free number < pv
    | otherwise         = getMinFree'  left start leftLen
    
    where (left, right) = partitionList xs pv
    
          -- pv is the partition value used to split xs, 
          -- the value assoicated with the mid-point index of xs
          pv            = computePartition start len
          
          -- the number of values < pv
          leftLen       = length left
          
          -- the number of values >= pv
          rightLen      = len - leftLen
          
          -- The value associated with index 0 in right
          rightStart    = start + leftLen
          
          -- The smallest free value exists in the right partition
          inRight       = rightStart == pv

-- Computes the partition value for use with getMinFree'
-- @param  start The start value
-- @param  len The length
-- @return The partition value
computePartition :: Int -> Int -> Int
computePartition start len = start + 1 + half len

-- Partitions a list of integers into 2 lists, one list containing values less than the partition value
-- and one list containing values greater than or equal to the partition value
-- @param  xs The list of numbers to partition
-- @param  pv The value to partition the list with
-- @return A tuple containing two lists, one list with all values less than the partition value (left partition)
-- and the other list all values greater than or equal to the partition value (right partition)
partitionList :: [Int] -> Int -> ([Int], [Int])          
partitionList xs pv = partition (< pv) xs   
                   
-- Combines a list of IO actions into a single action for use in the main method
-- @return A single action formed by binding all the actions together    
bindActions :: [IO ()] -> IO ()
bindActions = foldr (>>) $ Main.print ""

-- (Obviously) converts an Int to a String
-- @return The string representation of given Int
intToString :: Int -> String
intToString = show

-- Print a string to stdout, terminated with CRLF
-- @return the Action that prints the line
printLine :: String -> IO ()
printLine = putStrLn

-- Print a string to stdout, not ternimated with CRLF
-- @return The action that prints the line
print :: String -> IO ()
print = putStr

-- Prints the list of resuls
-- @param  xs List of SFNRes result tuples
-- @return An array of actions that print the results to stdout
printSmallestFreeNumbers :: [SFNRes] -> [IO ()]
printSmallestFreeNumbers xs = [printSmallestFreeNumber x | x <- xs]

-- Prints a single result
-- @param  sfn The SFNRes to write to stdout
-- @return Action that prints the result to stdout
printSmallestFreeNumber :: SFNRes -> IO ()
printSmallestFreeNumber sfn = printLine $ smallestFreeNumberToString sfn

-- Converts a SFNRes to a string, that can be written to stdout
-- @param  s The name of the list
-- @param  xs The list of natural numbers
-- @param  e The expected smallest free number
-- @param  a The computed smallest free number
-- @return A string containing the details of the list of numbers and the smallest free number
smallestFreeNumberToString :: SFNRes -> String
smallestFreeNumberToString (s, xs, e, a) = 
    s ++ " Expected: " ++ intToString e ++ 
    " Got: " ++ intToString a ++ " " ++ getResultStatus e a

-- Get a string indicating whether the expected value matches the actual value
-- @param  e The epxected value
-- @param  a The actual value
-- @return "Ok" if e == a, "Failed" otherwise
getResultStatus :: Int -> Int -> String
getResultStatus e a | e == a = "Ok"
                    | otherwise = "Failed"

-- Given a list of lists of numbers, find the smallest free number in eadh list
-- @param  xs The list of SFNReq to compute smallest free numbers for
-- @return List of SFNRes that contains the results
computeSmallestFreeNumbers :: [SFNReq] -> [SFNRes]
computeSmallestFreeNumbers xs = [getMinFree x | x <- xs]

-- Divide the given Int by 2, using integer division
half :: Int -> Int
half n = n `div` 2

 -- Creates a list of list of SFNReq's that can be used to test getMinFree
 -- @return A list of SFNReq's that can be used to test getMinFree
generateTestLists :: [SFNReq]
generateTestLists =
    [
        ("Empty", [], 0)
        ,("Single", [1], 0)
        ,("Basic", [0,1,2,3,5,7,9], 4)
        ,("Basic Left", [0,2,3,4,5,6,7,8,9], 1)
        ,("Unsorted", [9,0,4,6,1,3,5,2,8], 7)
        ,generateListMissingSingleNumber(1000,756)
        ,generateListMissingSingleNumber(1000,666)
        ,generateListMissingSingleNumber(1000000,456789)
    ] ++ generateListsMissingSingleNumber 100
    
-- Generates a list of natural numbers from 0 to n, excluding one number in the range
-- @param  n The upper bound of the list
-- @param  e The natural number to exclude from the list
-- @return A list of all natural numbers from 0 to n, excluding e
generateList :: (Int,Int) -> [Int]
generateList(n,e) = [x | x <- [0 .. n], x /= e]

-- Generate a list of tuples for use in generateTestLists of the form
-- (List Name, List Values, Expected Smallest Free Number)
-- @param  n The highest natural number in the lists
-- @return A list of SFNReq's, each missing one value in the range 0 - n
generateListsMissingSingleNumber :: Int -> [SFNReq]
generateListsMissingSingleNumber n = [generateListMissingSingleNumber(n,x) | x <- [0..n]]

-- Generate a single SFNReq 
-- @param  n The upper bound of the list
-- @param  e The natural number to exclude from the list
-- @return The SFNReq
generateListMissingSingleNumber :: (Int, Int) -> SFNReq
generateListMissingSingleNumber (n,e) = ("Missing " ++ intToString e, generateList(n,e), e)