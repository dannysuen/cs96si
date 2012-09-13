# Today's Topic: Functors

# A refresher on Type Classes

- Members of the `Ord` typeclass can be ordered. They implement `compare`, `(>)`, `(<)`, `(>=)`, `(<=)`, `max`, `min`.
- Members of the `Eq` typeclass implement `(==)` and `(/=)`
- The `Show` typeclass presents an interface for types whose values can be displayed as strings
- The `Read` typeclass is used whenever we need to convert a string to a value of some type
- Now we'r going to look at the `Functor` typeclass, which is basically for things that can be mapped over.
	
# The Functor Type Class
	
	class Functor f where
	  fmap :: (a -> b) -> f a -> f b
	  
* `f` is a type constructor that holds a parameter.
* A quick refresher: A type constructor is used to construct new types from given ones.

	data Tree a = Tip | Node a (Tree a) (Tree a)

* `Tree a` is a concrete type. `Tree` is a type constructor.

* `fmap` takes a function from one type to another and a functor applied with one type and returns a functor applied with another type

Since for lists, fmap is just map, we get the same results when using them on lists.

	map :: (a -> b) -> [a] -> [b]  
	ghci> fmap (*2) [1..3]  
	[2,4,6]  
	ghci> map (*2) [1..3]  
	[2,4,6]  

# List is an instance of the Functor typeclass

* The type signature of `map` is `map :: (a -> b) -> [a] -> [b]`
* That is, `map` takes a function from one type to another and a list of one type and returns a list of another type.
* map` is just an `fmap` that works only on lists.


		instance Functor [] where
			fmap = map

That's it! 

Notice how we wrote `Functor []` rather than instance `Functor [a]` where, because from `fmap :: (a -> b) -> f a -> f b`, we see that the `f` has to be a type constructor that takes one type. `[a]` is already a concrete type (of a list with any type inside it), while `[]` is a type constructor that takes one type and can produce types such as `[Int]`, `[String]` or even `[[String]]`.
[3](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)

# The Maybe type constructor

All from [LYAH](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)

Types that can act like a box can be functors. You can think of a list as a box that has an infinite amount of little compartments and they can all be empty, one can be full and the others empty or a number of them can be full. So, what else has the properties of being like a box? For one, the Maybe a type. In a way, it's like a box that can either hold nothing, in which case it has the value of Nothing, or it can hold one item, like "HAHA", in which case it has a value of Just "HAHA". Here's how Maybe is a functor.

	instance Functor Maybe where  
	    fmap f (Just x) = Just (f x)  
	    fmap f Nothing = Nothing  
Again, notice how we wrote instance Functor Maybe where instead of instance Functor (Maybe m) where, like we did when we were dealing with Maybe and YesNo. Functor wants a type constructor that takes one type and not a concrete type. If you mentally replace the fs with Maybes, fmap acts like a (a -> b) -> Maybe a -> Maybe b for this particular type, which looks OK. But if you replace f with (Maybe m), then it would seem to act like a (a -> b) -> Maybe m a -> Maybe m b, which doesn't make any damn sense because Maybe takes just one type parameter.

Anyway, the fmap implementation is pretty simple. If it's an empty value of Nothing, then just return a Nothing. If we map over an empty box, we get an empty box. It makes sense. Just like if we map over an empty list, we get back an empty list. If it's not an empty value, but rather a single value packed up in a Just, then we apply the function on the contents of the Just.

	ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
	Just "Something serious. HEY GUYS IM INSIDE THE JUST"  
	ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing  
	Nothing  
	ghci> fmap (*2) (Just 200)  
	Just 400  
	ghci> fmap (*2) Nothing  
	Nothing  

# Tree as a Functor Instance

	instance Functor Tree where  
	    fmap f EmptyTree = EmptyTree  
	    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)  
	
	ghci> fmap (*2) EmptyTree  
	EmptyTree  
	ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
	Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree  

# Either is an instance of the Functor Typeclass

	data Either a b = Left a | Right b  

The Functor typeclass wants a type constructor that takes only one type parameter but Either takes two. To resolve this, we can feed it only one parameter so it has one free parameter.

	instance Functor (Either a) where  
	    fmap f (Right x) = Right (f x)  
	    fmap f (Left x) = Left x  
	    
Why do we apply `f` to the value in `Either` only if the value is wrapped in `Right` and not `Left`?
You can think of Either a b as a computation, that may succeed and return b or fail with an error message a. It's only natural that the Functor instance won't touch the Left values, since when you map over the computation, if it fails, there's nothing to manipulate.	

TODO: maybe take out
"This also goes nicely with our box analogy if we think of the Left part as sort of an empty box with an error message written on the side telling us why it's empty."    

[4](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)




