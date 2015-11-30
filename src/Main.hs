module Main where

main :: IO ()
main = return ()

s1, t1, s2, t2, s3, t3 :: String
s1 = "HASKELL"
t1 = "PASCA-L"
s2 = "H-ASKELL"
t2 = "-PASC-AL"
s3 = "writers"
t3 = "vintner"

scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

-- return the optimal alignment of two strings
similarityScore :: String -> String -> Int
similarityScore [] _ = scoreSpace
similarityScore _ [] = scoreSpace
similarityScore x'@(x:xs) y'@(y:ys) = maxi case1 case2 case3 where
    case1 = similarityScore xs ys + colScore x y    -- two non-space chars
    case2 = similarityScore xs y' + colScore x '-'  -- non-space above, space below
    case3 = similarityScore x' ys + colScore '-' y  -- space above, non-space below
    maxi :: Int -> Int -> Int -> Int
    maxi a b c = max (max a b) c


colScore :: Char -> Char -> Int
colScore _ '-' = scoreSpace
colScore '-' _ = scoreSpace
colScore x y
    | x == y    = scoreMatch
    | otherwise = scoreMismatch

{- This was a misunderstanding; I thought here that similarityScore should
take the optimal alignment as parameters and simply count the score
similarityScore [] _ = 0
similarityScore _ [] = 0
similarityScore (s:ss) (t:ts)
    | s == '-' || t == '-' = scoreSpace + similarityScore ss ts
    | s == t    = scoreMatch + similarityScore ss ts
    | otherwise = scoreMismatch + similarityScore ss ts
-}

-- Given a list of pairs of two lists as the third parameter, cons
-- the first parameter to the first element of each pair (i.e. the first list
-- in the pairs), cons the second parameter to the second element of each pair (i.e. 
-- the other list in the pairs)
-- Take two heads and attach (cons) them at the beginning of each pair in the
-- list of pairs of lists given as third parameter, e.g.
-- > attachHeads '!' '?' [("hello", "can"), ("you", "hear")]
-- > [("!hello", "?can"), ("!you", "?hear")]
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]
