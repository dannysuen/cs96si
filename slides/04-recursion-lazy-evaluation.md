# Recursion

* Recursion is the idea of definining a function in terms of itself.

Let's take the factorial function as an example. Here are the definitions of the factorial of each of the first six counting numbers:

	Factorial of 6 = 6 × 5 × 4 × 3 × 2 × 1
	Factorial of 5 =     5 × 4 × 3 × 2 × 1
	Factorial of 4 = 		  4 × 3 × 2 × 1
	Factorial of 3 =     		  3 × 2 × 1
	Factorial of 2 =         		  2 × 1
	Factorial of 1 =                     1

You can see that the factorial of 6 (denoted as 5!) involves the 5!. In fact, 6! is just 6 * 5!

	factorial 0 = 1
	factorial n = n * factorial (n-1)

Indeed, we can see that the factorial of any number is just that number multiplied by the factorial of the number one less than it. There's one exception to this: if we ask for the factorial of 0, we don't want to multiply 0 by the factorial of -1 In fact, we just say the factorial of 0 is 1 (we define it to be so. It just is, okay?). So, 0 is the base case for the recursion: when we get to 0 we can immediately say that the answer is 1, without using recursion. 

We can summarize the definition of the factorial function as follows:

	The factorial of 0 is 1.
	The factorial of any other number is that number multiplied by the factorial of the number one less than it.

We can translate this directly into Haskell:
Example: Factorial function
	
	factorial 0 = 1
	factorial n = n * factorial (n-1)
	
As you can see, recursion nearly always comes in two parts: a base case and a recursive case.	
	
You can try it out by typing the following line into GHCI:

	> let { factorial 0 = 1; factorial n = n * factorial (n - 1) }
	
Let's look at what happens when you execute factorial 3:

	3 isn't 0, so we calculate the factorial of 2
		2 isn't 0, so we calculate the factorial of 1
			1 isn't 0, so we calculate the factorial of 0
				0 is 0, so we return 1.
			To complete the calculation for factorial 3, we multiply the current number, 1, by the factorial of 0, which is 1, obtaining 1 (1 × 1).
		To complete the calculation for factorial 3, we multiply the current number, 2, by the factorial of 1, which is 1, obtaining 2 (2 × 1 × 1).
	To complete the calculation for factorial 3, we multiply the current number, 3, by the factorial of 2, which is 2, obtaining 6 (3 × 2 × 1 × 1).

We can see how the result of the recursive call is calculated first, then combined using multiplication. Once you see how it can work, you rarely need to "unwind" the recursion like this when reading or composing recursive functions. Compilers have to implement the behaviour, but programmers can work at the abstract level, e.g., the relationship between factorial n and factorial (n-1).

[1](http://www.haskell.org/haskellwiki/Haskell/Lazy_evaluation)
# Recursion is especially useful for list-based operations

A lot of functions in Haskell turn out to be recursive, especially those concerning lists. Let us begin by considering the length function, that finds the length of a list:

Example: The recursive definition of length
	
	length :: [a] -> Int
	length []     = 0
	length (x:xs) = 1 + length xs

Let us explain the algorithm in English to clarify how it works. The type signature of lengths length tells that it takes any sort of list and produces an `Int`. The next line says that the length of an empty list is 0; and that, naturally, is the base case. The final line is the recursive case: if a list consists of a first element, `x`, and `xs`, the rest of the list, the length of the list is one plus the length of `xs`

# Another example of a recursive operation on a list

	maximum' :: (Ord a) => [a] -> a  
	maximum' [] = error "maximum of empty list"  
	maximum' [x] = x  
	maximum' (x:xs)   
	    | x > maxTail = x  
	    | otherwise = maxTail  
	    where maxTail = maximum' xs  
    
  or
  
	maximum' :: (Ord a) => [a] -> a  
	maximum' [] = error "maximum of empty list"  
	maximum' [x] = x  
	maximum' (x:xs) = max x (maximum' xs)    
        
    
Side note: As you can see, pattern matching goes great with recursion! Most imperative languages don't have pattern matching so you have to make a lot of if else statements to test for edge conditions.
	
# Another example of a recursive operation on a list
	
The "take" function takes a certain number of elements from a list.
	
	take' :: (Num i, Ord i) => i -> [a] -> [a]  
	take' n _  
	    | n <= 0   = []  
	take' _ []     = []  
	take' n (x:xs) = x : take' (n-1) xs  

# Another example of a recursive operation on a list

	reverse' :: [a] -> [a]  
	reverse' [] = []  
	reverse' (x:xs) = reverse' xs ++ [x] 


# Another example of a recursive operation on a list

"elem" takes an element and a list and sees if that element is in the list.

	elem' :: (Eq a) => a -> [a] -> Bool  
	elem' a [] = False  
	elem' a (x:xs)  
	    | a == x    = True  
	    | otherwise = a `elem'` xs   

# Don't get TOO excited about recursion

* Although it's very important to have a solid understanding of recursion when programming in Haskell, one rarely has to write functions that are explicitly recursive. Instead, there are all sorts of standard library functions which perform recursion for you in various ways, and one usually ends up using those instead. For example, a much simpler way to implement the factorial function is as follows:

Example: Implementing factorial with a standard library function
	
	factorial n = product [1..n]
	
Almost seems like cheating, doesn't it? :) This is the version of factorial that most experienced Haskell programmers would write, rather than the explicitly recursive version we started out with. Of course, the product function is using some list recursion behind the scenes[5], but writing factorial in this way means you, the programmer, don't have to worry about it.
[3](http://en.wikibooks.org/wiki/Haskell/Recursion#cite_note-1)

# Strict vs Lazy Evaluation
from (http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)

* _Lazy Evaluation_ means the evaluation of a given expression is delayed, or suspended, until its result is needed. "In consequence, arguments are not evaluated before they are passed to a function, but only when their values are actually used." [5](http://www.haskell.org/haskellwiki/Lazy_evaluation)
*  Furthermore, the ﬁrst time a suspended expression is evaluated, the result is memoized (i.e., cached) so that the next time it is needed, it can be looked up rather than recomputed.
* Lazy evaluation means expressions are only evaluated at the last pos

* F is only started once g tries to read some input, and only runs for long enough to deliver the output g is trying to read. Then f is suspended and g is run until it tries to read another input. As an added bonus, if g terminates without reading all of f’s output then f is aborted. F can even be a non-terminating program, producing an inﬁnite amount of output, since it will be terminated forcibly as soon as g is ﬁnished. This allows termination conditions to be separated from loop bodies - a powerful
modularisation
It makes it practical to modularise a program as a generator which
constructs a large number of possible answers, and a selector which chooses the
appropriate one. While some other systems allow programs to be run together in
this manner, only functional languages use lazy evaluation uniformly for every
function call, allowing any part of a program to be modularised in this way.
Lazy evaluation is perhaps the most powerful tool for modularisation in the
functional programmer’s repertoire.

[9](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf)

* "In strict languages, the arguments to a function
are evaluated before the body of the function. In lazy languages, arguments are evaluated in a demand-driven fashion; they are initially passed in unevaluated form and are evaluated
only when (and if!) the computation needs the results to continue." [2]
""

* Lazy evaluation is often more efficient, because values that are not actually used are not computed.

* Lazy evaluation is not feasible in imperative languages, because it's useful when evaluation can have side-effects. The side-effects may need to occur in a certain order. Therefore, lazy evaluation is only feasible in purely functional languages. [TODO verify]

* A disadvantage of lazy evaluation is that it's difficult to reason about the running time of a given program.

higher-order functions and lazy evaluation, can contribute greatly to modularity

Finite list of integers:
	
	let one_to_ten = [1..10]

Infinite list of integers: (can only use this construct in a lazy language)

	let positive_ints = [1..]


# An Example of Lazy Evaluation

	quickSort [] = []
	quickSort (x:xs) = quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

If we now want to find the minimum of the list, we can define

	minimum ls = head (quickSort ls)

Which first sorts the list and then takes the first element of the list. However, because of lazy evaluation, only the head gets computed. For example, if we take the minimum of the list `[2, 1, 3,]` quickSort will first filter out all the elements that are smaller than two. Then it does quickSort on that (returning the singleton list [1]) which is already enough. Because of lazy evaluation, the rest is never sorted, saving a lot of computational time.

[6](http://stackoverflow.com/questions/265392/why-is-lazy-evaluation-useful)

# References

[1](http://www.haskell.org/haskellwiki/Haskell/Lazy_evaluation)
[2](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)