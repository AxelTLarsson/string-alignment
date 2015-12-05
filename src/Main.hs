{-
String Alignment
Axel Larsson
dat11al1
-}
module Main where

import           Data.List (intersperse)

main :: IO ()
main = return ()

scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

-- Take two heads and attach (cons) them at the beginning of each pair in the
-- list of pairs of lists given as third parameter, e.g.
-- > attachHeads '!' '?' [("hello", "can"), ("you", "hear")]
-- > [("!hello", "?can"), ("!you", "?hear")]
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- the oposite to attachHeads - attaches at the end of the lists instead
attachTails :: a -> a -> [([a], [a])] -> [([a], [a])]
attachTails t1 t2 aList = [(xs ++ [t1], ys ++ [t2]) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\x -> valueFcn x == maxVal) xs
    where maxVal = maximum $ map valueFcn xs


type AlignmentType = (String, String)

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s t = do
    let list = optAlignments s t
        formattedList = map formatAlignment list
    putStrLn $ "There are " ++ show (length list) ++ " optimal alignments:\n"
    mapM_ putStrLn formattedList
    putStrLn $ "There was " ++ show (length list) ++ " optimal alignments."

formatAlignment :: AlignmentType -> String
formatAlignment (s,t) = intersperse ' ' s ++ '\n' : intersperse ' ' t ++ "\n"


-- memoized, fast version
similarityScore :: String -> String -> Int
similarityScore xs ys = cellScore (length xs) (length ys)
    where
        cellScore i j = table !! i !! j
        table = [[cell i j | j <- [0..]] | i <- [0..]]
        cell :: Int -> Int -> Int
        cell 0 0 = 0
        cell i 0 = scoreSpace + cellScore (i-1) 0
        cell 0 j = scoreSpace + cellScore 0 (j-1)
        cell i j = max3 left diag top
            where
                left = cellScore (i-1) j + scoreSpace
                top  = cellScore i (j-1) + scoreSpace
                diag = cellScore (i-1) (j-1) + match
                match = if x == y then scoreMatch else scoreMismatch
                x = xs !! (i-1)
                y = ys !! (j-1)

                max3 :: Int -> Int -> Int -> Int
                max3 a b c = max (max a b) c

-- memoized, fast version
optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = snd $ cell (length xs) (length ys)
    where
        cellScore i j = fst $ table !! i !! j
        table = [[cell i j | j <- [0..]] | i <- [0..]]

        cell :: Int -> Int -> (Int, [AlignmentType])
        cell 0 0 = (0, [([],[])])
        cell i 0 = (scoreSpace + cellScore (i-1) 0, [([xs !! (i-1)], "-")])
        cell 0 j = (scoreSpace + cellScore 0 (j-1), [("-", [ys !! (j-1)])])
        cell i j = (score, alignments)
            where
                score = fst . head $ maximaBy fst $ [left, diag, top]
                alignments = concatMap snd $ maximaBy fst $ [left, diag, top]
                left = (cellScore (i-1) j + scoreSpace,
                        attachTails x '-' $ cellPrefixes (i-1) j)
                top = (cellScore i (j-1) + scoreSpace,
                       attachTails '-' y $ cellPrefixes i (j-1))
                diag = (cellScore (i-1) (j-1) + match,
                        attachTails x y $ cellPrefixes (i-1) (j-1))
                match = if x == y then scoreMatch else scoreMismatch
                x = xs !! (i-1)
                y = ys !! (j-1)

                cellPrefixes :: Int -> Int -> [AlignmentType]
                cellPrefixes i' j' = snd $ cell i' j'
