# Today's Topic: `do` notation

quoted from [LYAH](http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe)

"Monads in Haskell are so useful that they got their own special syntax called do notation."

Do notation is just syntactic sugar to allow us to beatifully glue together (monadic values in sequence) (>>=) and (>>).

The reason we're learning about `do` notation, is that it's crucial to write readable, elegant, concise programs, and our goal is to experience the beauty of function programming.

http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe

# It's just syntactic sugar

* Note: "Syntactic sugar" is a computer science term that refers to syntax within a programming language that is designed to make things easier to read or to express.

It makes the language "sweeter" for humans to use: things can be expressed more clearly, more concisely, or in an alternative style that some may prefer.

Syntactic sugar adds no expressive power to the language (It doesn't let us do anything new, it just makes what we can already do easier to write.)
[1](http://en.wikipedia.org/wiki/Syntactic_sugar)


	do x
is the same as 

	x
,


	do v <- x
	   <stmts>

is the same as

	x >>= \v -> do <stmts>
,

	do let <dcls>
	   <stmts>

is the same as

	let <dcls> in do <stmts>
	
			   

In the following, note that the `x` and `<stmts>` must have the same alignment:	

	do x
	   <stmts>

is the same as	
	
	do x >> do <stmts>	



	
And now some identities:	

	do f x 
	
is the same as
	
	do v <- return x
	   f v
,

	do m 
	
is the same as
	
	do v <- m
	   return v
,

	do x <- m
       y <- f x
	   g y
	
is the same as
	
	do y <- do x <- m
	           f x 
       g y
       
# Examples

Remember, `do` expressions are just different syntax for chaining monadic values.

	foo :: Maybe String
	foo = Just 3  >>= (\x ->
	      Just "!" >>= (\y ->
	      Just (show x ++ y)))
	      
Note that because of the definition of `(>>=)` for the `Maybe a`

	(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
	(Just x) >>= f = f x
	Nothing >>= _ = Nothing
	
if any of the values in the chaining sequence are `Nothing`, the chaining stops and `Nothing` is returned.	
	      
do-notation lets elimina us eliminate the annoying lambdas

	foo :: Maybe String
	foo = do
		x <- Just 3
		y <- Just "!"	      
		Just (show x ++ y)
		
Again, note that if `x` or `y` were `Nothing`, the rest of the do-block would not be evaluated, and `Nothing` would be returned.	

# Examples

	ghci> Just 9 >>= (\x -> Just (x > 8))  
	Just True 	
	
The equivaent using `do` notation is

	marySue :: Maybe Bool  
	marySue = do   
	    x <- Just 9  
	    Just (x > 8) 	
	    
# Examples

quoted from [LYAH](http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe)

In do notation, when we bind monadic values to names, we can utilize pattern matching, just like in let expressions and function parameters. Here's an example of pattern matching in a do expression:

	justH :: Maybe Char  
	justH = do  
	    (x:xs) <- Just "hello"  
	    return x  

We use pattern matching to get the first character of the string "hello" and then we present it as the result. So justH evaluates to Just 'h'.

Side note: "When matching on a pattern in a function fails, the next pattern is matched. If the matching falls through all the patterns for a given function, an error is thrown and our program crashes. On the other hand, failed pattern matching in let expressions results in an error being produced right away, because the mechanism of falling through patterns isn't present in let expressions."

-- TODO: could include a note here on returning Nothing rather than allowing a failure.
-- could be a homework assignment… probs not.
