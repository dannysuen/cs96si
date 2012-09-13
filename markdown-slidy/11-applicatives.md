# Today's Topic: Applicatives

* Applicative functors are beefed up functors, represented in Haskell by the Applicative typeclass, found in the Control.Applicative module.

# The following is just a copy of LYAH, unedited

# Motivation for Applicative Functors

As you know, functions in Haskell are curried by default, which means that a function that seems to take several parameters actually takes just one parameter and returns a function that takes the next parameter and so on. If a function is of type a -> b -> c, we usually say that it takes two parameters and returns a c, but actually it takes an a and returns a function b -> c. That's why we can call a function as f x y or as (f x) y. This mechanism is what enables us to partially apply functions by just calling them with too few parameters, which results in functions that we can then pass on to other functions.

So far, when we were mapping functions over functors, we usually mapped functions that take only one parameter. But what happens when we map a function like *, which takes two parameters, over a functor? Let's take a look at a couple of concrete examples of this. If we have Just 3 and we do fmap (*) (Just 3), what do we get? From the instance implementation of Maybe for Functor, we know that if it's a Just something value, it will apply the function to the something inside the Just. Therefore, doing fmap (*) (Just 3) results in Just ((*) 3), which can also be written as Just (* 3) if we use sections. Interesting! We get a function wrapped in a Just!

ghci> :t fmap (++) (Just "hey")  
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
ghci> :t fmap compare (Just 'a')  
fmap compare (Just 'a') :: Maybe (Char -> Ordering)  
ghci> :t fmap compare "A LIST OF CHARS"  
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]  
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]  
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]  
If we map compare, which has a type of (Ord a) => a -> a -> Ordering over a list of characters, we get a list of functions of type Char -> Ordering, because the function compare gets partially applied with the characters in the list. It's not a list of (Ord a) => a -> Ordering function, because the first a that got applied was a Char and so the second a has to decide to be of type Char.

We see how by mapping "multi-parameter" functions over functors, we get functors that contain functions inside them. So now what can we do with them? Well for one, we can map functions that take these functions as parameters over them, because whatever is inside a functor will be given to the function that we're mapping over it as a parameter.

ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]  
But what if we have a functor value of Just (3 *) and a functor value of Just 5 and we want to take out the function from Just (3 *) and map it over Just 5? With normal functors, we're out of luck, because all they support is just mapping normal functions over existing functors. Even when we mapped \f -> f 9 over a functor that contained functions inside it, we were just mapping a normal function over it. But we can't map a function that's inside a functor over another functor with what fmap offers us. We could pattern-match against the Just constructor to get the function out of it and then map it over Just 5, but we're looking for a more general and abstract way of doing that, which works across functors.

# Meet the Applicative typeclass
It lies in the Control.Applicative module and it defines two methods, pure and <*>. It doesn't provide a default implementation for any of them, so we have to define them both if we want something to be an applicative functor. The class is defined like so:

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
This simple three line class definition tells us a lot! Let's start at the first line. It starts the definition of the Applicative class and it also introduces a class constraint. It says that if we want to make a type constructor part of the Applicative typeclass, it has to be in Functor first. That's why if we know that if a type constructor is part of the Applicative typeclass, it's also in Functor, so we can use fmap on it.

The first method it defines is called pure. Its type declaration is pure :: a -> f a. f plays the role of our applicative functor instance here. Because Haskell has a very good type system and because everything a function can do is take some parameters and return some value, we can tell a lot from a type declaration and this is no exception. pure should take a value of any type and return an applicative functor with that value inside it. When we say inside it, we're using the box analogy again, even though we've seen that it doesn't always stand up to scrutiny. But the a -> f a type declaration is still pretty descriptive. We take a value and we wrap it in an applicative functor that has that value as the result inside it.

A better way of thinking about pure would be to say that it takes a value and puts it in some sort of default (or pure) context—a minimal context that still yields that value.

The <*> function is really interesting. It has a type declaration of f (a -> b) -> f a -> f b. Does this remind you of anything? Of course, fmap :: (a -> b) -> f a -> f b. It's a sort of a beefed up fmap. Whereas fmap takes a function and a functor and applies the function inside the functor, <*> takes a functor that has a function in it and another functor and sort of extracts that function from the first functor and then maps it over the second one. When I say extract, I actually sort of mean run and then extract, maybe even sequence. We'll see why soon.

# Maybe as an Applicative
Let's take a look at the Applicative instance implementation for Maybe.

instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  
Again, from the class definition we see that the f that plays the role of the applicative functor should take one concrete type as a parameter, so we write instance Applicative Maybe where instead of writing instance Applicative (Maybe a) where.

First off, pure. We said earlier that it's supposed to take something and wrap it in an applicative functor. We wrote pure = Just, because value constructors like Just are normal functions. We could have also written pure x = Just x.

Next up, we have the definition for <*>. We can't extract a function out of a Nothing, because it has no function inside it. So we say that if we try to extract a function from a Nothing, the result is a Nothing. If you look at the class definition for Applicative, you'll see that there's a Functor class constraint, which means that we can assume that both of <*>'s parameters are functors. If the first parameter is not a Nothing, but a Just with some function inside it, we say that we then want to map that function over the second parameter. This also takes care of the case where the second parameter is Nothing, because doing fmap with any function over a Nothing will return a Nothing.

So for Maybe, <*> extracts the function from the left value if it's a Just and maps it over the right value. If any of the parameters is Nothing, Nothing is the result.

OK cool great. Let's give this a whirl.

ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing  
We see how doing pure (+3) and Just (+3) is the same in this case. Use pure if you're dealing with Maybe values in an applicative context (i.e. using them with <*>), otherwise stick to Just. The first four input lines demonstrate how the function is extracted and then mapped, but in this case, they could have been achieved by just mapping unwrapped functions over functors. The last line is interesting, because we try to extract a function from a Nothing and then map it over something, which of course results in a Nothing.

# Loads more on applicatives...
