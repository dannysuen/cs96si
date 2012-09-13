# Curried Functions

"Currying a a technique of transforming a function that takes multiple arguments (or an n-tuple of arguments) in such a way that it can be called as a chain of functions each with a single argument (partial application)." [1](http://en.wikipedia.org/wiki/Currying)

"Haskell functions can take functions as parameters and return functions as return values. A function that does either of those is called a higher order function. Higher order functions aren't just a part of the Haskell experience, they pretty much are the Haskell experience. It turns out that if you want to define computations by defining what stuff is instead of defining steps that change some state and maybe looping them, higher order functions are indispensable. They're a really powerful way of solving problems and thinking about programs." [2](http://learnyouahaskell.com/higher-order-functions#curried-functions)


# Curried Functions

"Every function in Haskell officially only takes one parameter. So how is it possible that we defined and used several functions that take more than one parameter so far? Well, it's a clever trick! All the functions that accepted several parameters so far have been curried functions."[2](http://learnyouahaskell.com/higher-order-functions#curried-functions)

"Putting a space between two things is simply function application. The space is sort of like an operator and it has the highest precedence."

For example, `add` takes two parameters and adds 'em together. 

	add :: Int -> Int -> Int
	add a b = a + b
		

All functions only take one parameter, so what actually happens in `add 4 5` is that `4` is applied to `add`, and the result of this application is a function that will accept one parameter (here, the `5`). We can rewrite the function signature in an equivalent form:

	add :: Int -> (Int -> Int)
	
First `add 4` is evaluated. Then it returns a function that takes in a single `Int`:	

	add4 :: Int -> Int
	add4 y = 4 + y

	
In summary, "if you define a function like f x y = bla, this is the same as f x = \y -> bla, which is the same as f = \x -> (\y -> bla). In other words f is a function which takes one argument, x, and returns another function which takes one argument, y, and then returns the actual result."
[8](http://stackoverflow.com/questions/3794371/haskell-currying-need-further-explanation)
	
Even if you don't understand the above waffle, just remember this. Given a function definition such as:

    foo :: int -> int -> int -> int
The type after the last arrow is what the function (eventually) returns when fully evalutated.

# Another Example

	multThree :: (Num a) => a -> a -> a
	multThree x y z = x * y * z
	
In `multThree 7 8 9`, first `7` is applied to multThree, because they're separated by a space. That creates a function that takes one parameter and returns a function. Then `8` is applied to that, which creates a function that will take a parameter and multiply it by 15. `9` is applied to that function and the result is `504`.

	ghci> let multTwoNumbersWithSeven = multThree 7
	ghci> multTwoNumbersWithSeven 8 9
	504
	ghci> let multWithSixtyThree = multTwoNumbersWithSeven 8
	ghci> multWithSixtyThree 9
	504


By calling functions with too few parameters, so to speak, we're creating new functions on the fly. What if we wanted to create a function that takes a number and compares it to 100? We could do something like this:

	compareWithHundred :: (Num a, Ord a) => a -> Ordering  
	compareWithHundred x = compare 100 x  

If we call it with 99, it returns a GT. Simple stuff. Notice that the x is on the right hand side on both sides of the equation. Now let's think about what compare 100 returns. It returns a function that takes a number and compares it with 100. Wow! Isn't that the function we wanted? We can rewrite this as:

	compareWithHundred :: (Num a, Ord a) => a -> Ordering  
	compareWithHundred = compare 100  

The type declaration stays the same, because `compare 100` returns a function. Compare has a type of `(Ord a) => a -> (a -> Ordering)` and calling it with `100` returns a `(Num a, Ord a) => a -> Ordering`.

[3](http://learnyouahaskell.com/higher-order-functions#curried-functions)


# Infix Operators

Infix operators are really just functions, and can also be defined using equations. For example, here is a definition of a list concatenation operator: 

(++)                    :: [a] -> [a] -> [a]
[]     ++ ys            =  ys
(x:xs) ++ ys            =  x : (xs++ys)

[Lexically, infix operators consist entirely of "symbols," as opposed to normal identifiers which are alphanumeric (§2.4). Haskell has no prefix operators, with the exception of minus (-), which is both infix and prefix.]

As another example, an important infix operator on functions is that for function composition: 

(.)                     :: (b->c) -> (a->b) -> (a->c)
f . g                   = \ x -> f (g x)

Side note: We can use any prefix function as a infix operator if we enclose it in \`backticks\` (not apostrophes).

For example, 

	x `elem` xs

[6](http://www.haskell.org/tutorial/functions.html)

# Sections

Since infix operators are really just functions, it makes sense to be able to partially apply them as well. In Haskell the partial application of an infix operator is called a section. For example:

	(x+)	=	\y -> x+y
	(+y)	=	\x -> x+y
	(+)	=	\x y -> x+y

These definitions would do just fine:

	inc = (+ 1)
	add = (+)
,

	divideByTen :: (Floating a) => a -> a  
	divideByTen = (/10)  

Calling, say, `divideByTen 200` is equivalent to doing `200 / 10`, as is doing `(/10) 200`. A function that checks if a character supplied to it is an uppercase letter:

	isUpperAlphanum :: Char -> Bool  
	isUpperAlphanum = (`elem` ['A'..'Z'])  

You can read `less = filter (< x) xs` aloud like "keep those in `xs` that are less than `x`". Although `(< x)` is just a shorthand for `(\y -> y < x)`, try reading that aloud!	

[6](http://www.haskell.org/tutorial/functions.html)
