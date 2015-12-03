module Main where
import Data.List (intersperse)

main :: IO ()
main = return ()

s1, t1, s2, t2, s3, t3, s4, t4, s5, t5, s6, t6 :: String
s1 = "HASKELL"
t1 = "PASCA-L"
s2 = "H-ASKELL"
t2 = "-PASC-AL"
s3 = "writers"
t3 = "vintner"
s4 = "writ-ers"
t4 = "vintner-"
s5 = "wri-t-ers"
t5 = "-vintner-"
s6 = "wri-t-ers"
t6 = "v-intner-"

scoreMatch, scoreMismatch, scoreSpace :: Int
scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

-- return the optimal alignment of two strings
similarityScore :: String -> String -> Int
similarityScore [] _ = scoreSpace
similarityScore _ [] = scoreSpace
similarityScore x'@(x:xs) y'@(y:ys) = max3 case1 case2 case3 where
    case1 = similarityScore xs ys + colScore (x,y)    -- two non-space chars
    case2 = similarityScore xs y' + colScore (x,'-')  -- non-space above, space below
    case3 = similarityScore x' ys + colScore ('-',y)  -- space above, non-space below
max3 :: Int -> Int -> Int -> Int
max3 a b c = max (max a b) c


colScore :: (Char, Char) -> Int
colScore (_, '-') = scoreSpace
colScore ('-', _) = scoreSpace
colScore (x, y)
    | x == y    = scoreMatch
    | otherwise = scoreMismatch

{- This was a misunderstanding; I thought here that similarityScore should
take the optimal alignment as parameters and simply count the score
-}
score :: AlignmentType -> Int
score ((s:ss), (t:ts))
    | s == '-' || t == '-' = scoreSpace + score (ss, ts)
    | s == t    = scoreMatch + score (ss, ts)
    | otherwise = scoreMismatch + score (ss, ts)
score (_, _) = 0


-- Given a list of pairs of two lists as the third parameter, cons
-- the first parameter to the first element of each pair (i.e. the first list
-- in the pairs), cons the second parameter to the second element of each pair (i.e.
-- the other list in the pairs)
-- Take two heads and attach (cons) them at the beginning of each pair in the
-- list of pairs of lists given as third parameter, e.g.
-- > attachHeads '!' '?' [("hello", "can"), ("you", "hear")]
-- > [("!hello", "?can"), ("!you", "?hear")]
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\x -> valueFcn x == maxVal) xs
    where maxVal = maximum $ map valueFcn xs


type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments x'@(x:xs) y'@(y:ys) = maximaBy score $ case1 ++ case2 ++ case3
    where case1 = attachHeads x y $ optAlignments xs ys
          case2 = attachHeads x '-' $ optAlignments xs y'
          case3 = attachHeads '-' y $ optAlignments x' ys
optAlignments [] (y:ys) = attachHeads '-' y $ optAlignments [] ys
optAlignments (x:xs) [] = attachHeads x '-' $ optAlignments xs []
optAlignments [] [] = [([],[])]

a3, a4, a5 :: AlignmentType
a3 = (s3, t3)
a4 = (s4, t4)
a5 = (s5, t5)


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s t = do
    let list = optAlignments s t
        formattedList = map formatAlignment list
    putStrLn $ "There are " ++ show (length list) ++ " optimal alignments:\n"
    mapM_ putStrLn formattedList
    putStrLn $ "There was " ++ show (length list) ++ " optimal alignments."

formatAlignment :: AlignmentType -> String
formatAlignment (s,t) = intersperse ' ' s ++ '\n' : intersperse ' ' t ++ "\n"