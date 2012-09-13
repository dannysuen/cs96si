# Today's Topic: Monoids
 

# A refresher on Typeclasses

A typeclass is a sort of interface that defines some behavior.

- For example, `Eq` is a typeclass. Other types may be members of this typeclass, which means that they implement `(==)` and `(/=)`.
- Members of the `Ord` typeclass can be ordered. They implement `compare`, `(>)`, `(<)`, `(>=)`, `(<=)`, `max`, `min`.
- The `Show` typeclass presents an interface for types whose values can be displayed as strings
- The `Read` typeclass is used whenever we need to convert a string to a value of some type
- Now we'r going to look at the `Functor` typeclass, which is basically for things that can be mapped over.

Let's say we want to define our own `Food` typeclass such that every member of the typeclass must implement the `pricePerPound` function:

	class Food food where
	  pricePerPound :: food -> Price
	  
Now let's create some instances of the Food typeclass. In order to be a member of the typeclass, they must implement `pricePerPound`.
	  

	data Fruit = Fruit { color :: String
                       , price :: Float
                       , weight :: Float
                       }

	data Steak = Steak

	instance Food Fruit where
	  pricePerPound fruit = (price fruit) / (weight fruit)

	instance Food Steak where
	  pricePerPound steak = 20.0

Now we want to create a function called `isExpensive` that returns `True` if the food costs more than $5 per pound.

	isExpensive :: Food f => f -> Bool
	isExpensive food = pricePerPound food > 5

Everything before the `=>` is called a *class constraint*.

If we try to apply `isExpensive` to any data type that is not a member of the `Food` typeclass, the compiler will produce a compile-time error.

if we hadn't included the type constraint, we could have passed in a `String` rather than a Food, and when we tried to apply `pricePerPound` to the `String`, it would raise a runtime error, because the `String` data type does implement `pricePerPound`.

# Monoids

"To put it bluntly, a monoid is a data type with an associative operation which combines two elements and a "zero" value which acts as neutral element for the operation. The Monoid captures this general concept:"

	class Monoid a where 
	  mempty  :: a
	  mappend :: a -> a -> a
	  mconcat :: [a] -> a
	  mconcat = foldr mappend mempty
	  
"The names of the class methods are highly suggestive; and indeed lists are a paradigmatic instance of Monoid, with [] as "zero"/mempty and (++) as mappend, the operation combining two elements. The third method, mconcat, is provided as bonus, and has a default implementation of mconcat = foldr mappend mempty which, naturally, is equivalent to concat for lists."	  

* The default implementation for `mconcat` is fine for most cases.

[1](http://en.wikibooks.org/wiki/Haskell/Monoids)
	
# `mappend` and `mempty`

Monoid Laws:

	mempty `mappend` x = x
	x `mappend` mempty = x
	(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

Haskell doesn't enforce these laws, so we as the programmer have to be careful that our instances do indeed obey them.

* A good candidates for `mappend` is function that takes two inputs and returns a third, all of the same type, like `(++)`, `(*)`, and `(+)`. 
	* Furthermore, we'll want to use `mappend` to combine three or more values and reduce them to a single result. The order in which we apply the binary function must not matter. 
	* That is, it doesn't matter if we do `(3 * 4) * 5` or `3 * (4 * 5)`. 
	* The same goes for `(++)`. `"la" ++ ("di" ++ "da")` equals `("la" ++ "di") ++ "da"`. 
	* `(-)` does not have this property, so we can't use it as a `mappend` function. e.g. `(5 - 3) - 4` and `5 - (3 - 4)` result in different answers.
* A good candidate for `mempty` is a value that we can `mappend` to an second value of the same type, where the output is identical to the second value. `mempty represents the identity value for a particular monoid.
	* `mempty` for the `mappend` function, `(+)` is `0`
	* `mempty` for the `mappend` function, `(*)` is `1`
	* `mempty` for the `mappend` function, `(++)` is `[]`
	
# What can be an instance of the Monoid typeclass?

* The Monoid typeclass definition doesn't take any type paramters
	* an example of a definition that takes type parameters is:

		class Functor f where
		  	fmap :: (a -> b) -> f a -> f b
		  	
		Here the f takes a type parameter `a` or `b`. The Functor and Applicative typeclasses require their instances to be type constructors which take one parameter, for instance, `Maybe a` or `List a`.
		
	* the Monoid class definition contains no such type parameters:	  	
		class Monoid a where 
		  mempty  :: a
		  mappend :: a -> a -> a
		  mconcat :: [a] -> a
		  mconcat = foldr mappend mempty		  	
  * Therefore, instances of the Monoid typeclass must not take any type parameters. They must be concrete types, like `Integer` or `List` TODO: List?	
	
# Examples	
	
LYAH: It's worth noting that the decision to name mappend as it's named was kind of unfortunate, because it implies that we're appending two things in some way. While ++ does take two lists and append one to the other, * doesn't really do any appending, it just multiplies two numbers together. When we meet other instances of Monoid, we'll see that most of them don't append values either, so avoid thinking in terms of appending and just think in terms of mappend being a binary function that takes two monoid values and returns a third.

# Any and All as Monoids

from LYAH:

The Any newtype constructor is an instance of Monoid:

	newtype Any = Any { getAny :: Bool }  
		deriving (Eq, Ord, Read, Show, Bounded)  

	instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)  

	ghci> getAny $ Any True `mappend` Any False  
	True  
	ghci> getAny $ mempty `mappend` Any True  
	True  
	ghci> getAny . mconcat . map Any $ [False, False, False, True]  
	True  
	ghci> getAny $ mempty `mappend` mempty  
	False  

"The other way for Bool to be an instance of Monoid is to kind of do the opposite: have && be the binary function and then make True the identity value. Logical and will return True only if both of its parameters are True. This is the newtype declaration, nothing fancy:"

	newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  

	instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)  

	ghci> getAll $ mempty `mappend` All True  
	True  
	ghci> getAll $ mempty `mappend` All False  
	False  
	ghci> getAll . mconcat . map All $ [True, True, True]  
	True  
	ghci> getAll . mconcat . map All $ [True, True, False]  
	False  

# The Ordering Monoid

The Ordering type can have three values: `LT`, `EQ` and `GT`:

	ghci> 1 `compare` 2  
	LT  
	ghci> 2 `compare` 2  
	EQ  
	ghci> 3 `compare` 2  
	GT  

instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT  

When we mappend two Ordering values, the one on the left is kept, unless the value on the left is `EQ`, in which case the right one is the result. The identity is `EQ`. 

This implementation "resembles the way we alphabetically compare words. We compare the first two letters and if they differ, we can already decide which word would go first in a dictionary. However, if the first two letters are equal, then we move on to comparing the next pair of letters and repeat the process."

"OK, so how is this monoid useful? Let's say you were writing a function that takes two strings, compares their lengths, and returns an Ordering. But if the strings are of the same length, then instead of returning EQ right away, we want to compare them alphabetically. One way to write this would be like so:"

	lengthCompare :: String -> String -> Ordering  
	lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a  


Using our undestanding that Ordering is a monoid, we can write this more elgantly:

	import Data.Monoid  
  
	lengthCompare :: String -> String -> Ordering  
	lengthCompare x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)  

	ghci> lengthCompare "zen" "ants"  
	LT  
	ghci> lengthCompare "zen" "ant"  
	GT  

We can expand this function to also compare for the number of vowels and set this to be the second most important criterion for comparison:

	import Data.Monoid  
  
	lengthCompare :: String -> String -> Ordering  
	lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")  

The Ordering monoid is cool because it allows us to compare things by many different criteria and order those critera by importance.

# Maybe the monoid

"One way is to treat Maybe a as a monoid only if its type parameter a is a monoid as well and then implement mappend in such a way that it uses the mappend operation of the values that are wrapped with Just. We use Nothing as the identity, and so if one of the two values that we're mappending is Nothing, we keep the other value. Here's the instance declaration:"

	instance Monoid a => Monoid (Maybe a) where  
	    mempty = Nothing  
	    Nothing `mappend` m = m  
	    m `mappend` Nothing = m  
	    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  

Notice the class constraint. It says that Maybe a is an instance of Monoid only if a is an instance of Monoid. 

	ghci> Nothing `mappend` Just "andy"  
	Just "andy"  
	ghci> Just LT `mappend` Nothing  
	Just LT  
	ghci> Just (Sum 3) `mappend` Just (Sum 4)  
	Just (Sum {getSum = 7})  

The `Maybe` monoid "comes in use when you're dealing with monoids as results of computations that may have failed. Because of this instance, we don't have to check if the computations have failed by seeing if they're a Nothing or Just value; we can just continue to treat them as normal monoids."