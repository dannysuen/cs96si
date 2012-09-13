# Today's Topic: Monads

# This is totally unfinished

quoted from [LYAH](http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe)

When we first talked about functors, we saw that they were a useful concept for values that can be mapped over. Then, we took that concept one step further by introducing applicative functors, which allow us to view values of certain data types as values with contexts and use normal functions on those values while preserving the meaning of those contexts.

In this chapter, we'll learn about monads, which are just beefed up applicative functors, much like applicative functors are only beefed up functors.

# A refresher on Functors and Applicatives

When we started off with functors, we saw that it's possible to map functions over various data types. We saw that for this purpose, the Functor type class was introduced and it had us asking the question: when we have a function of type a -> b and some data type f a, how do we map that function over the data type to end up with f b? We saw how to map something over a Maybe a, a list [a], an IO a etc. We even saw how to map a function a -> b over other functions of type r -> a to get functions of type r -> b. To answer this question of how to map a function over some data type, all we had to do was look at the type of fmap:

fmap :: (Functor f) => (a -> b) -> f a -> f b  
And then make it work for our data type by writing the appropriate Functor instance.

Then we saw a possible improvement of functors and said, hey, what if that function a -> b is already wrapped inside a functor value? Like, what if we have Just (*3), how do we apply that to Just 5? What if we don't want to apply it to Just 5 but to a Nothing instead? Or if we have [(*2),(+4)], how would we apply that to [1,2,3]? How would that work even? For this, the Applicative type class was introduced, in which we wanted the answer to the following type:

(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
We also saw that we can take a normal value and wrap it inside a data type. For instance, we can take a 1 and wrap it so that it becomes a Just 1. Or we can make it into a [1]. Or an I/O action that does nothing and just yields 1. The function that does this is called pure.

Like we said, an applicative value can be seen as a value with an added context. A fancy value, to put it in technical terms. For instance, the character 'a' is just a normal character, whereas Just 'a' has some added context. Instead of a Char, we have a Maybe Char, which tells us that its value might be a character, but it could also be an absence of a character.

It was neat to see how the Applicative type class allowed us to use normal functions on these values with context and how that context was preserved. Observe:

ghci> (*) <$> Just 2 <*> Just 8  
Just 16  
ghci> (++) <$> Just "klingon" <*> Nothing  
Nothing  
ghci> (-) <$> [3,4] <*> [1,2,3]  
[2,1,0,3,2,1]  
Ah, cool, so now that we treat them as applicative values, Maybe a values represent computations that might have failed, [a] values represent computations that have several results (non-deterministic computations), IO a values represent values that have side-effects, etc.

# Monads

Monads are a natural extension of applicative functors and with them we're concerned with this: if you have a value with a context, m a, how do you apply to it a function that takes a normal a and returns a value with a context? That is, how do you apply a function of type a -> m b to a value of type m a? So essentially, we will want this function:

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b 

If we have a fancy value and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function? This is the main question that we will concern ourselves when dealing with monads. We write m a instead of f a because the m stands for Monad, but monads are just applicative functors that support >>=. The >>= function is pronounced as bind.

When we have a normal value a and a normal function a -> b it's really easy to feed the value to the function — you just apply the function to the value normally and that's it. But when we're dealing with values that come with certain contexts, it takes a bit of thinking to see how these fancy values are fed to functions and how to take into account their behavior, but you'll see that it's easy as one two three.


# The `Maybe` monad

* System libraries define a `Monad` instance for `Maybe`


    instance  Monad Maybe  where
        (Just x) >>= k = k x
        Nothing >>= _  = Nothing
        return = Just
        fail _ = Nothing


* You can use `Nothing` to indicate failure
    * Might have a bunch of functions to extract fields from data


    extractA :: String -> Maybe Int
    extractB :: String -> Maybe String
    ...
    parseForm :: String -> Maybe Form
    parseForm raw = do
        a <- extractA raw
        b <- extractB raw
        ...
        return (Form a b ...)

    * Threads success/failure state through system as `IO` threaded
      World
    * Since Haskell is lazy, stops computing at first `Nothing`
    
# In Conclusion

* Monads can be thought of as composable computation descriptions.
* The essence of Monad is thus
	* separation of composition timeline from the composed computations execution timeline?
	* as well as the ability of computation to implicitly carry extra data, as pertaining to the computation itself, in additionto its _one_ output that it _will_ produce when run.
* This lends monads to supplementing pure calculations with features like IO, common environment or state, and to preprocessing of computation (simplification, optimization, etc.)
    
# Homework

* Read the "Walk the Line" example in [Learn You a Haskell](http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe)
