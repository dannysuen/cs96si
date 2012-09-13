
# Today

* Higher order functions are the bread and butter of functional programming.
	* Map, filter
* Lambdas 
* Closures
* Functions as first-class objects

# Function composition
* Here's a refresher.    

	countLowerCase :: String -> Int
	countLowerCase = length . filter isLower

* The "`.`" operator provides function composition

	(f . g) x = f (g x)

* Function composition can be used almost like Unix pipelines

    process = countLowercase . toPigLatin . extractComments . unCompress

[1](240h slides)

# Map

The action of the map function is very simple: it takes a list and applies some function to every element of a list, obtaining a new list. It's defined as:

	map :: (a -> b) -> [a] -> [b]
	map _ [] = []  
	map f (x:xs) = f x : map f xs  
	
Some examples:

	ghci> map (+3) [1,5,3,1,6]  
	[4,8,6,4,9]  
	ghci> map (++ "!") ["BIFF", "BANG", "POW"]  
	["BIFF!","BANG!","POW!"]  
	ghci> map (replicate 3) [3..6]  
	[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
	ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
	[[1,4],[9,16,25,36],[49,64]]  
	ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  
	[1,3,6,2,2]  	
	
Now suppose you want a list of the ﬁrst ten squares:

	ghci> map (\x -> x*x ) [1..10]
	[1,4,9,16,25,36,49,64,81,100]
	
Wait--what's that `\x` doing there?? It's the start of a function that we made up on the spot and didn't have to give a name to. In other words, it's an _anonymous function_, also known as a _lambda abstraction_.

[2](https://docs.google.com/viewer?a=v&q=cache:U4vRVOZ0fr0J:www2.math.ou.edu/~dmccullough/teaching/f06-6833/haskell/map_filter.pdf+&hl=en&gl=us&pid=bl&srcid=ADGEESjcCmKgfg6iUrCYZDU2hrb_sIy114D4nZZv-jR6f1A1nNXq3uuABBRRzQY8odNMZn2LdHQiJNLGs6d4L2Obw7ohBuc72WaHzwsdSAdRJrqsM6rZNi0vsiOsj5w_zDJGW-LdB6pX&sig=AHIEtbTStYEchrse3COaAQd8twCJghF9KA)

# List Comprehensions

List comprehension is really nothing more than map and filter in a friendlier format:

	[ f(x) | x <- list ]

is really
	
	map f list

and

	[ x | x <- list, P(x) ]

is really
	
	filter P list

For example,

	[ (firstCh, secondCh) | firstCh <- "The first string.",
	secondCh <- "The second string.", isLower firstCh, isUpper secondCh,
	toUpper firstCh == secondCh ]

(don't think too hard. It's a dummy function. The result is `[('t','T'),('t','T')]`)

can be written as the composition

	filter (\pair -> toUpper (fst pair) == (snd pair) )
	( concat
	( map (\ch1 -> map (\ch2 -> (ch1, ch2)) (filter isUpper "The second string." ) )
	(filter isLower "The first string.") ) )

Clearly, this is much easier to understand when written using list comprehension.
[2]

# Lambda abstraction

* Sometimes you want to name the arguments but not the function
* Haskell allows anonymous functions through *lambda abstraction*
    * The notation is `\`*variable(s)* `->` *body* (where `\` is
      pronounced "lambda")


* That is, the inputs come after the `\`, and the function body goes after the `->`. We can apply a lambda abstraction to a value by appending the value. For instance, `(\x -> x * x) 7` would give `49`.

* We most often use lambdas when we need to create a function "in place". For example:


		countLowercaseAndDigits :: String -> Int
		countLowercaseAndDigits = length . filter (\c -> isLower c || isDigit c)


`filter` is defined as 

	filter :: (a -> Bool) -> [a] -> [a]  
	filter _ [] = []  
	filter p (x:xs)   
	    | p x       = x : filter p xs  
	    | otherwise = filter p xs  
	
The first paramter is a function. We could name the function, as in

	isLowerOrIsDigit :: Char -> Bool
	isLoverOrIsDigit c = isLower c || isDigit c	
but if we never plan to use it again, we may as well define it only where we use it, without naming it. So instead we use the lambda abstraction, `(\c -> isLower c || isDigit c)`.

* Lambda abstractions can deconstruct values with patterns, e.g.:

		… (\(Right x) -> x) …

    * But note that guards or multiple bindings are not allowed
    * Patterns must have the right constructor or will get run-time error

Some more usage examples of filter:

	ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]  
	[5,6,4]  
	ghci> filter (==3) [1,2,3,4,5]  
	[3]  
	ghci> filter even [1..10]  
	[2,4,6,8,10]  
	ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]  
	[[1,2,3],[3,4,5],[2,2]]  
	ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
	"uagameasadifeent"  
	ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  
	"GAYBALLS" 

* "Mapping and filtering is the bread and butter of every functional programmer's toolbox."
* Another example: "Find the largest number under 100,000 that's divisible by 3829. To do that, we'll just filter a set of possibilities in which we know the solution lies."


		largestDivisible :: (Integral a) => a  
		largestDivisible = head (filter p [100000,99999..])  
	    	where p x = x `mod` 3829 == 0 
	
[5](http://learnyouahaskell.com/higher-order-functions#curried-functions)

# Closures

A closure is just a fancy name for how lexical scope interacts with first-class functions.

Loosely speaking, a closure is an expression with certain values filled in. These values are filled in from an outer scope.

For example, 
	
	f x = (\y -> x + y) 
	
If we call `f` as `f 2`, we produce a function that takes an argument and adds `2` to it. Here we have "closed over" the value of `x`, which was in the lexical scope of the function call of f.

Said another way, a closure is a function that can reference state in another function.

Said another way, a closure stores namespace at the point it was defined.

	plus-x :: Int -> Int
	plus-x y = let x = 5 in
           x + y

	let x = 3
	in plus-x 1

The result is `6` rather than `4`.
	
Because Haskell is lazy, `x^y` is a closure, because the free variables `x` and `y` are closed over (their values are bound from an outer scope), and the the inner expression, which waits to be evaluated, lives in a different inner scope. The `x^y` expression has filled-in values from an outer scope, and is therefore a closure.	

* Closures are just an implementation detail, and understanding them is not of fundamental importance in learning Haskell. I only mention them because they're absolutely crucial to having first-class functions. (TODO take out the following? If you couldn't close over part of the environment where a function is defined, we wouldn't be able to seed functions with values defined not in the scope of the function.)

# More on Closures (TODO: take out closures altogether?)

from (http://www.haskell.org/pipermail/beginners/2009-July/002067.html)

A closure is essentially a binding, *together with* the enclosing
environment---the idea being that the binding may refer (via free
variables) to things in its environment.  The ability to have closures
is absolutely crucial to having first-class functions.

For example, consider this function:

  mkAdder :: Int -> (Int -> Int)
  mkAdder y = \x -> x + y

mkAdder takes an Int as an argument, and returns a function (Int ->
Int) as a result.  But take a look at the function it returns: \x -> x
+ y has a free variable (y) which refers to its environment.  So
really what you get when you call mkAdder with a particular argument
(say, 3) is a closure, containing the function \x -> x + y together
with the environment (y = 3).  

Of course, hopefully you have realized that mkAdder is really just
(+), written in a funny way!  So this isn't a contrived example;
closures are quite fundamental in Haskell.

With that said, on some level the idea of a closure is really just an
implementation detail---I wouldn't say that understanding it is of
fundamental importance in learning Haskell.  But learning things never
hurts (except when it does).



# Functions as first-class entities

Haskell functions are first class entities, which means that they

- can be given names
- can be the value of some expression
- can be members of a list
- can be elements of a tuple
- can be passed as parameters to a function
- can be returned from a function as a result

(quoted from Davie's Introduction to Functional Programming Systems using Haskell.)

For example, the `isUpper` function below is passed as an argument to `filter`.

	ghci> filter isUpper "Some Character String"
	"SCS"

As another example, `add5` is a function that returns another function:
	
	add5 :: Int -> (Int -> Int)
	add5 y = add 5 y

As another example, when I test the functions you write for your assignments, I define all the tests, which are all functions, I store them in a list, and then I execute them using a special mapping function over the list.

	main = runTests [testAdd5, testStartsWithW, testCurryTwo]

# What's the difference between higher order functions and first-class functions?

* "higher-order" describes a mathematical concept of functions that operate on other functions

For example: `map toUpper "elephant"`

* "First-class" is a computer science term that describes programming language entities that have no restriction on their use.

Specifically, first-class functions:
- can be given names
- can be the value of some expression
- can be members of a list
- can be elements of a tuple
- can be passed as parameters to a function
- can be returned from a function as a result


# References

http://www.joelonsoftware.com/items/2006/08/01.html
and okasaki...
http://en.wikibooks.org/wiki/Haskell/List_processing 