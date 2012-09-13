# Today's Topic: Folds

A **fold** applies a function to a list in a way similar to map, but accumulates a single result instead of a list.


# Motivation for folds
`
 sum :: [Integer] -> Integer
 sum []     = 0
 sum (x:xs) = x + sum xs`
 `
 
 `
 product :: [Integer] -> Integer
 product []     = 1
 product (x:xs) = x * product xs
 `
 
 `
 concat :: [[a]] -> [a]
 concat []     = []
 concat (x:xs) = x ++ concat xs
 `
 
There is a certain pattern of recursion common to all of these examples. This pattern is known as a fold, possibly from the idea that a list is being "folded up" into a single value.
 
 The Standard Prelude defines four fold functions: `foldr`, `foldl`, `foldr1` and `foldl1`.
 
### foldl 
 
	foldl            :: (a -> b -> a) -> a -> [b] -> a
	foldl f acc []     =  acc
	foldl f acc (x:xs) =  foldl f (f acc x) xs
  

foldl folds the list up from the left side. The binary function is applied between the starting value and the head of the list. That produces a new accumulator value and the binary function is called with that new value and the next element, etc. 
 
	foldl (max) 0 [3,10,14]
is the same as

	(max (max (max 0 3) 10) 14)
 
The "l" in "foldl" stands for left-asociative. We evaluate the elments from left to right. 

Let's walk through this step-by-step:

Initially, `acc` is 0, `x` is 3, and `xs` is [10, 14].
The new accumulator becomes `max 0 3`.
The new `x` becmoes 10.
The new `xs` becomes [14].
The new accumulator becomes `max (max 0 3) 10`
The new `x` becomes [14].
The new `xs` becmoes [].
The new accumulator becomes `max (max (max 0 3) 10) 14`
`x:xs` is now `[]`
The result of `foldl f acc [] is acc`, and so we return `max (max (max 0 3) 10) 14`.

As another example, 

	sum :: [Int] -> Int
	sum numbers = foldl (+) 0 numbers
`sum [1, 2, 3, 4]` gives 10.

	product :: [Int] -> Int
	product numbers = foldl (*) 1 numbers
`sum [1, 2, 3, 4]` gives 24.

* Note that `foldl` is *tail recursive*. 
* _Tail recursion_ means that each recursive call changes an accumulator, such that when we reach the bottommost recursive call, we have the result stored in the accumulator and we can return. You never need to refer to old stack frames. 
* In contrast, *head recursion* means that the recursive call depends on result of all deeper recursive calls, so you have to save the current stack frame as you go and evaluate all deeper recursive calls. As soon as you reach the bottommost recursive call, you have to "pop back out" to return the results of the all the recursive calls as you compute the result. Of course, tail-recursion is more space and time-efficient. `foldr` is head recursinve.
 
### foldr
The right-associative foldr folds up a list from the right, that is, it walks from the last to the first element of the list and applies the given function to each of the elements and the accumulator, the initial value of which has to be set:

	foldr :: (a -> b -> b) -> b -> [a] -> b
	foldr f acc []     = acc
	foldr f acc (x:xs) = f x (foldr f acc xs)
The first argument is a function with two arguments, the second is a "zero" value for the accumulator, and the third is the list to be folded.

Let's look at the `max` example again.


whereas `foldl (max) 0 [3,10,14]` is left-associative (it's evaluated as `(max (max (max (max 0 3) 10) 14)`), `foldr (max) 0 [3,10,14]` is right-associative. It's evaluated as 
`max 0 (max 3 (max 10 14))`

The step-by-step trace of `foldl` will be more difficult than the `foldr` example because it's head-recursive, so at each step we have to save the current stack frame and compute all the rest of the values until we "pop back out". We don't simply update an accumulator and return it when we hit the bottommost recursive call, as we do in the case of tail-recursion.

Let's walk through this step by step:

- Initially, `acc` is 0, `x` is 3, and `xs` is [10, 14].
- Now we call `max` on 3 and the result of a call to `foldr` (we'll name it call_2).
- In call_2, `acc` is 0, `x` is 10, and 'xs' is [14]. The result of call_2 is the result when `max` is called on 10 and the result of a call to `foldr` (we'll name it call_3).
- In call_3, `acc` is still 0, `x` is 14, and `xs` is `[]`, so the result of call_3 is is the result when `max` is called on 14 and the result of a call to `foldr` (we'll name it call_4).
- In call_4, `(x:xs)` is `[]`, so the result is `acc`, which is 0.
- call_3 is then `(max 14 0)`
- call_2 is then `max 10 (max 14 0)`.
- The initial call is then `(max 3 (max 10 (max 14 0))`.


Technical Note: The left associative fold is tail-recursive, that is, it recurses immediately, calling itself. For this reason the compiler will optimise it to a simple loop, and *it will then be much more efficient than foldr*. However, Haskell is a lazy language, and so the calls to f will by default be left unevaluated, building up an expression in memory whose size is linear in the length of the list, exactly what we hoped to avoid in the first place. To get back this efficiency, there is a version of foldl which is strict, that is, it forces the evaluation of f immediately, called foldl'. Note the single quote character: this is pronounced "fold-ell-tick". A tick is a valid character in Haskell identifiers. foldl' can be found in the library Data.List (which can be imported by adding import Data.List to the beginning of a source file). As a rule of thumb you should use foldr on lists that might be infinite or where the fold is building up a data structure, and foldl' if the list is known to be finite and comes down to a single value. foldl (without the tick) should rarely be used at all, unless the list is not too long, or memory usage isn't a problem.

# Distinction between `foldr` and `foldl`

TODO: Is this example helpful?
 
It helps to understand the distinction between foldr and foldl. Why is foldr called "fold right"?

Initially I thought it was because it consumed elements from right to left. Yet both foldr and foldl consume the list from left to right.

foldl evaluates from left to right (left-associative)
foldr evaluates from right to left (right-associative)
We can make this distinction clear with an example that uses an operator for which associativity matters. We could use a human example, such as the operator, "eats":

foodChain = (human : (shark : (fish : (algae : []))))

foldl step [] foodChain
  where step eater food = eater `eats` food  -- note that "eater" is the accumulator and "food" is the element

foldl eats [] (human : (shark : (fish : (algae : []))))
  == foldl eats (human `eats` shark)                              (fish : (algae : []))
  == foldl eats ((human `eats` shark) `eats` fish)                (algae : [])
  == foldl eats (((human `eats` shark) `eats` fish) `eats` algae) []
  ==            (((human `eats` shark) `eats` fish) `eats` algae)
The semantics of this foldl is: A human eats some shark, and then the same human who has eaten shark then eats some fish, etc. The eater is the accumulator.

Contrast this with:

foldr step [] foodChain
    where step food eater = eater `eats` food.   -- note that "eater" is the element and "food" is the accumulator

foldl `eats` [] (human : (shark : (fish : (algae : []))))
  == foldl eats (human `eats` shark)                              (fish : (algae : []))))
  == foldl eats (human `eats` (shark `eats` (fish))               (algae : [])
  == foldl eats (human `eats` (shark `eats` (fish `eats` algae))) []
  ==            (human `eats` (shark `eats` (fish `eats` algae) 
The semantics of this foldr is: A human eats a shark which has already eaten a fish, which has already eaten some algae. The food is the accumulator.

Both foldl and foldr "peel off" eaters from left to right, so that's not the reason we refer to foldl as "left fold". Instead, the order of evaluation matters.
 
Check out foldl.com and foldr.com. 
 
# foldl and strictness
For summing elements, `foldl` looks better than foldr on the face of things, because at every step through the list, the function we're folding with can see both of its arguments.

But there's a catch.

None of the intermediate results are visible to the caller of foldl, so the caller cannot force those results to be evaluated.

In other words, we're still building up a big thunk!

 [9](http://www.scs.stanford.edu/11au-cs240h/notes/par-slides.html#(9))
 
A strict left fold
This function is defined for us in Data.List, and it has the same type as foldl:

{-# LANGUAGE BangPatterns #-}

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f = go
  where
    go !z (x:xs) = go (f z x) xs
    go !z  _     = z
The crucial difference lies with the strictness annotation, which forces each intermediate result to be evaluated.

* foldl' may as well be called strictFold
* foldl' is pronounced "fold prime"
* foldl' is so-called because it's closely related to `foldl`: it's a foldl where the accumulator is evaluated at every step, so that large thunks (code waiting to be evaluted) don't build up.

 
# Don't use foldl
It should be clear by now that plain old foldl is very rarely useful.

In fact, I've never personally found a situation where it was the right kind of fold to use.

If you find yourself thinking "I need a left fold", it's safe to assume that foldl' is what you'll need.  

# References
 
http://en.wikibooks.org/wiki/Haskell/List_processing
 