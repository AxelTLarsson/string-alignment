module Main where

main :: IO ()
main = return ()

s1, t1, s2, t2 :: String
s1 = "HASKELL"
t1 = "PASCA-L"
s2 = "H-ASKELL"
t2 = "-PASC-AL"

scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

similarityScore :: String -> String -> Int
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (s:ss) (t:ts)
    | s == t    = scoreMatch + similarityScore ss ts
    | s == '-' || t == '-' = scoreSpace + similarityScore ss ts
    | otherwise = scoreMismatch + similarityScore ss ts
