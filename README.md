## Homework for [ITMO's Haskell course](https://github.com/jagajaga/FP-Course-ITMO)
Full text of hometasks is available via links in headers (in Russian, with input and output examples)
### [Task 1.](https://hackmd.io/s/rJXa7ir9Z#) Haskell basics
#### Block 1. Simple functions
* Order3 -- orders tuple of 3
* highestBit -- returns highest pow of 2, less or equal than n
* smartReplicate -- replicates each element N times, where N is the element
* contains -- returns lists of lists, contains given element

#### Block 2. Pattern matching
* removeAt -- removes element by the index
* collectEvery -- removes every k elements from the list
* stringSum -- sum of all numbers in given string
* mergeSort -- merge sort

#### Block 3. Algebraic data types
* Days type. Supported functions: nextDay, afterDays, isWeekend, daysToParty (numbers of days till friday)
* Monster and Knights. Turn-based fight between monster and knight (he has first turn)
* Vectors. Supported functions: length, vector sum, scalar and vector multiplication, distance
* Natural number. Supported functions: +, -, \*, toInteger, fromInteger, compare. Implementing instanceof Eq, Ord, Num. (without using deriving)
* Binary Trees. Supported functions: isEmpty, size, find in tree (contract: given search tree), insert into serach tree, fromList

#### Block 4. Foldable
* Implenet instaceof foldable for Tree
* splitOn -- splits list into lists by element

#### Block 5. Monoids
* maybeConcat -- concatenating list of Maybe list
* Implement semigroup and monoid for Identity, NonEmpty list, Tree

### [Task 2.](https://hackmd.io/s/r19Io9Ys-#) Types in Haskell
#### Block 1. Monads.
* Arithmetic expressions. Supported operations: const, +, -, \*, div, pow
* Typesafe partial functions and instaceof Category for them. (Definition and lists of supported functions in document, with comments in English)

#### Block 2. Non-determinism
* Calculate all binary numbers given length
* Calculate Combinations n k
* Calculate all permutations of list

#### Block 3. Non-determinism
* Proofs of equivalence different Monad definitions
* Make types Identity, Either, Tree, Const, (,) instaceof Functor, Applicative, Foldable, Traversable

#### Block 4. Parsers
* Tasks from [Parser Combinators: Basics](http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf)
* Tasks from [Parser Combinators: Implementing simple parser](http://www.seas.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf)
* Make Parser instanceof Monad. Create and simplify let expressions parser.
