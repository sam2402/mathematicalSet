# Mathematical Set

##### How to run Mathematical Set:
- Make sure you have the GHCi complier installed (https://www.haskell.org/platform/)
- Fork this repository
- Run the command below:  
`ghci mathsSet.hs`  

##### How to use Mathematical Set:
- Use the functions defined in the mathsSet.hs file to make sets and perform actions on those sets
- They are documented in comments above the function definitions

##### Notes on the design:
- The set is implemented as a binary tree.
- Take a look at the function `removeSet`. Using three auxiliary functions, it removes an item from a binary tree.
- I think I have implemented the function `fromList` quite elegantly.
- The three auxiliary functions are not designed to be called by the user.
