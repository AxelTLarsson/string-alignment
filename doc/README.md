## Summary

In this assignment you will use Haskell to solve a fundamental theoretical problem from combinatorics. On the way you will learn to use memoization. The assignment is to be completed individually.

## Introduction

A major part of molecular biology deals with analyzing and comparing the genetic material (DNA, RNA, proteins, chromosomes, genes, etc.) of different organisms. Enormous amounts of information have to be collected, stored, and processed; fortunately, many kinds of biomolecular data share a common underlying sequential structure. For instance, the primary structure of a protein is a one-dimensional chain of amino acid residues, and hence expressible as a string over a finite alphabet. In the same way, the linear sequence of bases in a DNA molecule can be represented by a string over the alphabet {A,C,G,T}, and RNA by a string over the alphabet {A,C,G,U}. Efficient string algorithms have therefore turned out to be of great importance for solving combinatorial problems related to modern molecular biology.

One example of such a problem is the string alignment problem. An alignment of two strings is a way of placing one string above the other to illustrate how parts of the strings are related. Given two strings s and t, an alignment is obtained by inserting spaces into s and t so that the characters of the resulting strings can be put in one-to-one correspondence to each other, as in the following example where the symbol '-' is used to represent an inserted space:
```
H A S K E L L
P A S C A - L
```

Spaces may also be added to the beginning and the end of the strings, but a space in one string is never allowed to be aligned with a space in the other (for obvious reasons). Thus, another valid way to align the two strings is:
```
H - A S K E L L
- P A S C - A L
```

The length of an alignment is the number of columns it contains, so the first alignment has length 7 while the second alignment has length 8. Looking more closely, we see that in the first alignment, there are 3 matches, 3 mismatches, and 1 space; in the second, there are 3 matches, 2 mismatches, and 3 spaces. Which of the two alignments is "better"?

There is no definite answer, as this depends on the application, i.e., on how we choose to penalize mismatches and inserted spaces. This means that in some cases, the first alignment would be considered better than the second, while the second alignment would be preferred in other cases. The parameters that specify the optimization criteria will be referred to as scoreMatch, scoreMismatch, and scoreSpace.

One application of the string alignment technique is to identify an unknown virus. By aligning its RNA sequence to various known RNA sequences stored in a database (one at a time), and rejecting those with a similarity score lower than a certain threshold, one can select a small set of candidate RNA sequences to be subjected to further, more detailed comparisons. Another use of string alignments is when different laboratories that are working on obtaining the DNA sequence of a certain gene want to compare their results, an alignment will show where their results differ. Finally, alignments between DNA sequences from a set of similar species provide an indication of how the species are related to each other. This information can then be used to construct an evolutionary tree which describes how they have evolved from an assumed common ancestor. There are many other uses of string alignments in molecular biology as well as in other fields such as computer science, coding theory, dendrochronology, stratigraphic analysis, forensics, and speech recognition.

The difficulty involved in aligning strings is that the total number of possible alignments is exponential in the lengths of the input strings. A brute-force approach (generate all possible alignments, evaluate each one, and return all those with the highest score) is therefore useless for strings of sizes that occur in practice. For example, there are more than 10^764 possible alignments for two strings of length 1000. But it is possible to use a bottom-up method that takes advantage of the structure of the problem, and in this assignment, you will write a Haskell program that solves the string alignment problem more efficiently.

## Problem specification

Formally, the string alignment problem is defined as follows.

**Input**: Two strings s and t, and values for scoreMatch, scoreMismatch, and scoreSpace.
**Output**: All optimal alignments between s and t.

Or in terms of a Haskell type signature:
```haskell
optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
```

In order to be able to compare different alignments, a scoring scheme is required. For this reason, we define the score of an alignment as the sum of its column scores, where a column score is calculated from the characters in that column. If one of the characters is a space, the column score is equal to scoreSpace; otherwise, it is equal to either scoreMatch or scoreMismatch depending on whether the characters are identical or not.
In the example above, if we let
```
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2
```

then the score of the first alignment is -2 and the score of the second alignment is -5.

An optimal alignment is an alignment with the highest possible score. The score of such an alignment is called the similarity score of the two strings. Note that there can be more than one optimal alignment.

## Preparation

Read [here](http://cs.lth.se/index.php?id=56481) about a related problem, the maximal common subsequence problem. This problem is also decribed in section 19.6 of the Thomson's textbook, 2nd ed. (and in section 20.6 in 3rd ed.)

## Assignment

### 1. General understanding of the problem

An application of the string alignment problem to computer science is the fact that the maximal common subsequence problem (MCS) is a special case of the string alignment problem.
Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?

### 2. Haskell programming

a.) Write a Haskell function
```haskell
similarityScore :: String -> String -> Int
similarityScore string1 string2
```

that returns the score of the optimal alignment of the two strings string1 and string2. If you need to, consult the Hint section below.

b.) Explain what the following Haskell function does.
```haskell
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]
```

c.) Write a Haskell function
```haskell
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs
```

which generalizes the maximum function in two respects:

1. The "value" of an element is defined by a function supplied as a parameter.
1. Instead of just one element, the result is a list of all maximum elements.

For example, maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].

d.) Let
```haskell
type AlignmentType = (String,String).
```

Write a Haskell function
```haskell
optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2
```

which returns a list of all optimal alignments between string1 and string2. (Hint: Follow the same pattern as you did in part a., and make use of the functions defined in parts b. and c.)


e.) Write a Haskell function
```haskell
outputOptAlignments string1 string2
```

that prints all optimal alignments between string1 and string2 to the screen in a neat and easy-to-read fashion.

Example:

Let
```
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
```

Then
```
Main> similarityScore string1 string2
```
should return the value
```
-5
```
and
```
Main> optAlignments string1 string2
```

should return
```
[("writ-ers","vintner-"), ("wri-t-ers","-vintner-"), ("wri-t-ers","v-intner-")]
```

Finally,
```
Main> outputOptAlignments string1 string2
```

should yield something like
```
There are 3 optimal alignments:

w r i t - e r s
v i n t n e r -

w r i - t - e r s
- v i n t n e r -

w r i - t - e r s
v - i n t n e r -
```


### 3. Program optimization

The technique for speeding up MCS by storing intermediate results in a table can be applied to the string alignment problem as well. To avoid unnecessary recomputations, we could store the similarity scores between the various suffixes of string1 and string2 in a table, i.e., a list of lists.

Modify the definition of mcsLength to make the similarityScore and optAlignments functions in exercise 2 more efficient. You will need to redefine the auxiliary functions used by mcsLength and the table's initial values (that is, the values in the first "row" and the first "column"). As in mcsLength, the elements in the similarityScore table should be of the type Int. For optAlignments, the elements should be of the type (Int, [AlignmentType]) where the second element contain the optimal alignments themselves and the first element is the score for these alignments, i.e. the corresponding similarity score.

If you have succeeded in your optimization it should take at most a few seconds to correctly evaluate:
```
optAlignments "aferociousmonadatemyhamster" "functionalprogrammingrules"
```

Before you start coding either of the table based implementations you should convince yourself that you can correctly construct a small table of the correct type by hand.

Note: mcsLength stores optimal solutions for all prefixes of the input strings. If you decide to use this left-to-right ordering in your solution, you will probably need an attachTails function when constructing the optimal alignments.

Suggestion: As an intermediate step you might want to write a version of optAlignments where you only store the optimal alignments in the table and (inefficiently) do the necessary scoring calculation directly in the local auxilliary function. The elements in the table in this version will thus be of the type [AlignmentType]. Your final optimized solution will then be a combination of this version and the solution for similarityScore.

## Hint

Observe that every alignment can start in exactly one of three ways:

1. Two non-space characters
1. Non-space above, space below
1. Space above, non-space below

In an optimal alignment of two strings (x:xs) and (y:ys) beginning as in (1), xs has to be optimally aligned with ys. (If not, then a better-than-optimal solution could be obtained by combining the optimal alignment between xs and ys with the first column. Contradiction!) Therefore, the corresponding score is equal to the similarity score of xs and ys plus the first column's score, which is either scoreMatch or scoreMismatch.

If an optimal solution starts like in (2), xs is optimally aligned with (y:ys) and x is paired with a '-', and analogously for optimal alignments of type (3).

Thus, the following recursive relation holds for the similarity score of two non-empty strings:
```haskell
sim((x:xs),(y:ys)) = max {sim(xs,ys) + score(x,y),
                          sim(xs,(y:ys)) + score(x,'-'),
                          sim((x:xs),ys) + score('-',y)}
```

where
```haskell
score(x,'-') = score('-',y) = scoreSpace
score(x,y) = scoreMatch, if x == y
             scoreMismatch, if x /= y
```
