{-
    "1,200.00 + 5.2\n" -> "1200.00  + 5.2"

    1. remove commas
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe
import Data.List.Split

cleanInput :: [Char] -> [Char]
cleanInput xs = c
 where a = lines xs -- removes \n -> one element list
       b = head a   -- remove the one element
       c = (\xs -> [x | x  <- xs, x /= ',']) b -- remove commas

-- splitInput :: T.Text -> [T.Text]
-- splitInput = T.splitOn " "

data Operations = Addition | Subtraction | Multiplication | Division | Modulus
  deriving (Show, Eq)

getSymbol:: Maybe Operations -> Maybe Char
getSymbol = fmap getSymbol'

getSymbol' :: Operations -> Char
getSymbol' Addition = '+'
getSymbol' Subtraction = '-'
getSymbol' Multiplication = '*'
getSymbol' Division = '/'
getSymbol' Modulus = '%'

getOperation :: Foldable t => t Char -> Maybe Operations
getOperation str
  | '+' `elem` str = Just Addition
  | '-' `elem` str = Just Subtraction
  | '*' `elem` str = Just Multiplication
  | '/' `elem` str = Just Division
  | '%' `elem` str = Just Modulus
  | otherwise = Nothing

getNums :: Operations -> p -> [T.Text]
getNums Addition _ = T.splitOn "+" "123+456"
getNums Subtraction _ = T.splitOn "-" "123-456"
getNums Multiplication _ = T.splitOn "*" "123*456"
getNums Division _ = T.splitOn "/" "123/456"
getNums Modulus _ = T.splitOn "%" "123%456"


-- operation, operator, string
-- bla :: Operations -> Maybe Char -> [Char]
-- bla Addition _ str = T.SplitOn 
-- bla operation operator str = 

main :: IO ()
main = do
  print "Enter equation such as:"
  print "2 + 2"
  print "or"
  print "3 * 3"
  -- get input
  userInput <- getContents
  -- clean it
  let cleaned = cleanInput userInput
  let operation = getOperation cleaned
  let b = getSymbol operation
  -- let c = getNums operation
  print b



{-
    operator = Find +, -, *, /, %
    numLHS = get left of operator
    numRHS = get right of operator
    convert numLHS & numRHS to numbers
    perform operation

-}
