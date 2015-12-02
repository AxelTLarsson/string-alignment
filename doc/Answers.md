## Question 1
An application of the string alignment problem to computer science is the fact that the maximal common subsequence problem (MCS) is a special case of the string alignment problem.
Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?

### Answer
One can use the string alignment algorithm with the ```scoreMismatch``` and ```scoreSpace``` parameters set to 0 to get the MCS solution.


## Question 2
Explain what the following function does:
```haskell
attachHeads :: Ord b => (a -> b) -> [a] -> [a]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs,ys) <- aList]
```

### Answer
Take two heads and attach (cons) them at the beginning of each pair in the
list of pairs of lists given as third parameter, e.g:
```
ghci> attachHeads '!' '?' [("hello", "can"), ("you", "hear")]
ghci> [("!hello", "?can"), ("!you", "?hear")]
```