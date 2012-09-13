# Today's Topics: Typeclasses, data constructors, type synonyms, and `newtype`

# Type Variables

Head takes a list of any type and returns the first element. What's its type?

	ghci> :t head  
	head :: [a] -> a  
	
What's the `a`? It can't be a type, because types are written in capital case. 

Rather, it's a _type variable_. That means `a` can be of any type.

Functions that have type variables are called *polymorphic functions*.

The type declaration of head states that it takes a list of any type and returns one element of that same type.

We use different letters when we want to refer to two items that may have different types:

	ghci> :t fst  
	fst :: (a, b) -> a  

[1](http://learnyouahaskell.com/types-and-typeclasses)

# Typeclasses

A typeclass is a sort of interface that defines some behavior.

- For example, `Eq` is a typeclass. Other types may be members of this typeclass, which means that they implement `(==)` and `(/=)`.
- Members of the `Ord` typeclass can be ordered. They implement `compare`, `(>)`, `(<)`, `(>=)`, `(<=)`, `max`, `min`.

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

TODO: what if both the Food and the Fruit are members of Show? do we take the more specific implementation?

# Type Synonyms

	type String = [Char]

We've introduced the type keyword. The keyword might be misleading to some, because we're not actually making anything new (we did that with the data keyword), but we're just making a synonym for an already existing type.

It's often a good idea to create type synonyms when we want to ensure the data type represents what we want. For example, rather than:

	data Fruit = Fruit { color :: String
                       , price :: Float
                       , weight :: Float
                       }

we could include:

	type Dollars = Float
	type Pounds = Float

	data Fruit = { color :: Color
	             , price :: Dollars
	             , weight :: Pounds
	             }
to make it clear that we're using the American system of measurement.

Just like we can partially apply functions to get new functions, we can partially apply type parameters and get new type constructors from them. Just like we call a function with too few parameters to get back a new function, we can specify a type constructor with too few type parameters and get back a partially applied type constructor. If we wanted a type that represents a map (from `Data.Map`) from integers to something, we could either do this:

	type IntMap v = Map Int v  
Or we could do it like this:

	type IntMap = Map Int  
Either way, the `IntMap` type constructor takes one parameter and that is the type of what value that corresponds to each Integer key. 

# newtype

In addition to the familiar data keyword, Haskell provides us with another way to create a new type, using the newtype keyword.

The purpose of a newtype declaration is to rename an existing type, giving it a distinct identity.

	newtype Fahrenheit = Fahrenheit Float
		deriving (Eq, Ord, Show, Num, Fractional)

	newtype Celsius = Celsius Float
		deriving (Eq, Ord, Show, Num, Fractional)

	far2cel :: Fahrenheit -> Celsius
	far2cel (Fahrenheit far) = Celsius $ (5 / 9) * (far - 32)

	cel2far :: Celsius -> Fahrenheit
	cel2far (Celsius cel) = Fahrenheit $ (cel * (9 / 5)) + 32


If we had used a data constructor or a type synonym, the compiler would let us mix up Fahrenheit and Celsius. Using the `type` keyword, both we and the compiler know that `Fahrenheit` and `Float` refer to the same type.

In contrast, the newtype keyword exists to hide the nature of a type. The compiler treats Fahrenheit as a different type from Float.

[3](http://necrobious.blogspot.com/2009/03/fun-example-of-haskells-newtype.html)
[4](http://book.realworldhaskell.org/read/using-typeclasses.html)

# What's the difference between `data`, `type`, and `newtype`?

* If you just want your type signatures to look cleaner and be more descriptive, you probably want type synonyms, using the `type` keyword. 
	* `type Dollars = Float`
* If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you're looking for a `newtype`. 
	* `newtype Fahrenheit = Fahrenheit Float`
* And if you want to make something completely new, odds are good that you're looking for the `data` keyword.
	* `data Tree a = Tip | Node a (Tree a) (Tree a)`

# Superclasses and instance contexts

* One class may require all instances to be members of another
    * Class `Eq` contains '==' and '/=' methods, while
    `Ord` contains `<`, `>=`, `>`, `<=`, etc.
    * It doesn't make sense to have an `Ord` instance not also be an
      `Eq` instance
    * `Ord` declares `Eq` as a superclass, using a context


			class Eq a => Ord a where
				(<), (>=), (>), (<=) :: a -> a -> Bool
				a <= b = a == b || a < b -- default methods can use superclasses
            ....

    * Don't need to write superclass restrictions in contexts--any
      function with an `Ord` dictionary can lookup the `Eq` dictionary
    * Incidentally, can add `deriving (Eq, Ord)` to `data`
      declarations

# Deriving Typeclasses

	data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
		deriving (Eq, Ord, Show, Read, Bounded, Enum)
	
Because it's part of the Show and Read typeclasses, we can convert values of this type to and from strings.

	ghci> Wednesday  
	Wednesday  
	ghci> show Wednesday  
	"Wednesday"  
	ghci> read "Saturday" :: Day  
	Saturday  

Because it's part of the Eq and Ord typeclasses, we can compare or equate days.

	ghci> Saturday == Sunday  
	False  
	ghci> Saturday == Saturday  
	True  
	ghci> Saturday > Friday  
	True  
	ghci> Monday `compare` Wednesday  
	LT  
It's also part of `Bounded`, so we can get the lowest and highest day.

	ghci> minBound :: Day  
	Monday  
	ghci> maxBound :: Day  
	Sunday  

It's also an instance of `Enum`. We can get predecessors and successors of days and we can make list ranges from them!

	ghci> succ Monday  
	Tuesday  
	ghci> pred Saturday  
	Friday  
	ghci> [Thursday .. Sunday]  
	[Thursday,Friday,Saturday,Sunday]  
	ghci> [minBound .. maxBound] :: [Day]  
	[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday] 	

[3](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)

# Homework

Read sections _Algebraic data types intro_ and _Record Syntax_ in [chapter 8 of Learn You a Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types)

[RWH]: http://book.realworldhaskell.org/
[Platform]: http://hackage.haskell.org/platform/
[GHCdoc]: http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html
[GHCI]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html
[Hoogle]: http://www.haskell.org/hoogle/
[DMR]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5
[DMRWiki]: http://www.haskell.org/haskellwiki/Monomorphism_restriction
[Awkward]: http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf
[default]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4