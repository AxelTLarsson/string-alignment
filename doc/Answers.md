## Question 1
An application of the string alignment problem to computer science is the fact that the maximal common subsequence problem (MCS) is a special case of the string alignment problem.
Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?

### Answer
One can use the string alignment algorithm with the ```scoreMismatch``` and ```scoreSpace``` parameters set to 0 to get the MCS solution.
