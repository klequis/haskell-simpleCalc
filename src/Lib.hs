module Lib
    ( someFunc
    ) where

import Data.List.Split

myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

someFunc :: IO ()
someFunc = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)

