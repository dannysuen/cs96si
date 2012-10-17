
# Why take CS96si?

* **You enjoy programming**
  * Learn new, surprising, and effective programming techniques. You will think about programming in new ways.
* Mastery  of basic functional programming techniques
	* Higher order functions, function composition, curried functions, lambdas, folds
	* Recursion, lazy evaluation
	* Modularity, abstraction, 
	* Composable computation in Haskell (Monoids, Functors, Applicatives, Monads)

# Administrivia

* Prereqs: cs106A and cs106B
* No Haskell experience necessary
* No midterm, no final
* Extensive weekly assignments 
* Credit based on effort
* Fourteen late days. (count 'em. Four-teen!)
* **No set office hours, but please PLEEASE make an appointment with me whenever you want to talk.**

# What is Functional Programming?

* Say what you want done rather than how you want it done.

### Imperative Programming

Summing the integers 1 to 10 in Java. 

	total = 0;
	for (i = 0; i <= 10; ++i)
       total = total + i;

### Functional Programming
Summing the integers 1 to 10 in Haskell. 
      
	sum [1..10]

# What is Functional Programming?

The **key** to understanding the definiton of functional programming is that FP is based on one constraint. Specifically, that when you call a function twice with the same input, it must return the same output both times.

Really?? An entire programming paradigm is built on that simple idea?

_Really_.

# What is Functional Programming

From this single constraint, all other constraints follow.

Specifically, the requirement of "same input, same output" results in all the other distinctive qualities of functional programming:

- Functions don't have side-effects
- Every function’s output must only depend on its input. 
- No state changes
- Data is immutable
- Computation is achieved by evaluating expressions rather than executing a series of tasks
- Style is declarative
- Sequencing is abstracted

_Behold…._
  

# What is Functional Programming?
#### Same input, same output

* If a function is called twice with the same parameters, it _must_ return the same result.
  * This concept is known as **_Referential Transparency_**
  * An expression is said to be _referentially transparent_ if the expression can be replaced with its value without changing the behavior of a program (in other words, yielding a program that has the same effects and output on the same input.

# Examples of Referential Transparency (same input, same output)

* All arithmetic expressions are referentially transparent. `5 * 5` can be replaced by `25`.
      
# Examples of Referential Transparency (same input, same output)      

* `today()` is _not_ referentially transparent, as if you evaluate it and replace it by its value (say, "Jan 1, 2001"), you don't get the same result as you will if you run it tomorrow. This is because it depends on a state (the time).

# Examples of Referential Transparency (same input, same output)        

* **Assignments are not transparent.**
* For instance, the C expression `x = x + 1` changes the value assigned to the variable `x`. Let's say `x` is 5. Evaluating the expression once yields `6`. Evaluating the expression again yields `7`. And so, any assignment that changes the existing value is not referentiall transparent, because it is not free of side-effects. Here, the side effect is incrementing `x`.

# Wait WHAT??

No variable assignment?? But that's the bread and butter of imperative programming! How can we transform any data if we can't change state??    


# Examples of Referential Transparency (same input, same output)  

* Yes, in functional programming, assignment is discouraged in favor of single assignment, also called initialization. Single assignment is an example of name binding and differs from assignment as described in this article in that it can only be done once, usually when the variable is created; no subsequent re-assignment is allowed. Once created by single assignment, named values are not variables but immutable objects.

* For instance, calling a function such as `int plus_one(int x) {return x+1;}` is referentially transparent, as it will not implicitly change the input `x` and thus has no such side effects. When we call the function, we initialize the value of `x`, and it does not change. A new immutable value is the function output.  


# What is Functional Programming?

- Functions don't have side-effects
- Every function’s output must only depend on its input. 
- No state changes
- Data is immutable
- Computation is achieved by evaluating expressions rather than executing a series of tasks
- Style is declarative
- Sequencing is abstracted

# What is Functional Programming?

#### No state changes

* We have seen that referentially transparent functions do not have side effects that modify internal state or make other changes that aren’t visible in the function’s return value. _No state can change._
* Imperative programming, on the other hand, focuses on giving the computer a sequence of tasks. While executing the tasks, it can change state.
* Rather than emphasizing changes in state, functional programming emphasizes the application of functions. The functions operate on immutable data
* Operations on immutable data are useful. For instance, `int plus_one(int x) {return x+1;}`

[2](http://docs.python.org/howto/functional.html)

# What is Functional Programming?
#### It happens to be declarative

* Because we have rejected the idea that values can change (in a certain, limited, technical sense), then almost by accident you end up forcing the programmer to write programs that are more declarative, because a large part of imperative programming is describing how variables change, and you can no longer do that! so it turns out that functional programming - particularly, programming in a functional language - tends to give more declarative code.


[8](reference SO)

# What is Functional Programming?

#### Declarative. Evaluate expressions rather than execute a sequence of tasks

* Imperative programming languages are so-called because they consist of sequences of actions (commands). 
	* A definition of "imperative" is "giving an authoritative command; peremptory:" as in _the bell pealed again, a final imperative call._.	
	* The programmer quite explicitly tells the computer how to perform a task, step-by-step. 

* Functional programming languages work differently. Rather than performing actions in a sequence, they _evaluate expressions_.
[3](http://www.haskell.org/haskellwiki/Why_Haskell_matters)

* Imperative is the traditional "step by step recipe" approach while declarative is more "this is what i want, now you work out how to do it".

* In functional programming, you don't tell the computer what to do, but rather, what stuff _is_. 

> Functional programming is like describing your problem to a mathematician. 
> 
> Imperative programming is like giving instructions to an idiot.

[1](arcus, #scheme on Freenode)


# What is Functional Programming?
#### A different programmer focus
* This slide is my favorite.
* When you write in a functional style, you'll notice that you focus on data input and transformation, rather than process.
  
# What is Functional Programming?

## Let's get fundamental. FP abstracts sequencing.
 
- There are two areas that are fundamental to programming a computer - *resource management* and *sequencing*. 
- *Resource management* (allocating registers and memory) has been the target of vast abstraction, most new languages (imperative as well as functional) have implemented garbage collection to remove resource management from the problem, and lets the programmer focus on the algorithm instead of the book-keeping task of allocating memory. 
- *Sequencing* has also undergone some abstraction, although not nearly to the same extent. Imperative languages have done so by introducing new keywords and standard libraries. For example, most imperative languages have special syntax for constructing several slightly different loops, you no longer have to do all the tasks of managing these loops yourself. But imperative languages are based upon the notion of sequencing - they can never escape it completely. The only way to raise the level of abstraction in the sequencing area for an imperative language is to introduce more keywords or standard functions, thus cluttering up the language. This close relationship between imperative languages and the task of sequencing commands for the processor to execute means that imperative languages can never rise above the task of sequencing, and as such can never reach the same level of abstraction that functional programming languages can.
- In functinoal programming, the sequencing task is removed. You only care what the program is to compute not how or when it is computed. 

[4](http://www.haskell.org/haskellwiki/Why_Haskell_matters)

# What is Functional Programming?
### In conlusion

We have seen that the single requirement that the requirement of "same input, same output" results in all the other distinctive qualities of functional programming:

- Functions don't have side-effects
- Every function’s output must only depend on its input. 
- No state changes
- Data is immutable
- Computation is achieved by evaluating expressions rather than executing a series of tasks
- Style is declarative
- Sequencing is abstracted


# What is a Functional Language?

* A functional language is one that _supports_ and _encourages_ the functional style.


# Disadvantages of functional design

- Steeper learning curve
- At times data structures are more difficult to implement
- Performance can be worse.

# Downsides of Functional Programming

#### Steeper learning curve. Functional programming can be a mind-bender.

For instance, let's write a function that pulls out every 3rd element of a list.

Imperative (in C)

	int counter = 0;
	for (int i = 0; i < list.size; i++) {
	  if (counter % 3 == 0) 
	     new_list.push(list[i])
	}

That's easy enough. Set a counter, move to the next element, increment the counter, check to see if you're at the Nth element and so on.

The functional equivalent is not so easy. We can't change the value of a counter, so we must keep track of where we are in the list by some changing input and output of a recursive call.
Here we use a recursive call such that each time the call is run, the first n-1 elements are ignored, and the nth element is concatenated to the list.

Functional (in Haskell)

	every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


The latter method might seem more confusing, and that's because it is. Functional programming can be a mind-bender, which is one reason why Lisp, Scheme, and Haskell have never really surpassed C, C++, and Java in commercial popularity
[5](http://stackoverflow.com/questions/602444/what-is-functional-declarative-and-imperative-programming)

This example also demonstrates that many functional data structures are considered more difficult to design and implement than their imperative counterparts in large part because they are immutable. However, functional data structures can be as capable and asymptotically as fast as imperative data structures.

# Downsides of Functional Programming

#### Performance

Imperative languages provide more intimate control over the exact way in which a computation is carried out (that is, until sufficiently smart compiler appears that is able to derive the C equivalent from the Haskell one-liner, all by itself. In fact, the gap in performance between functional languages and their more traditional cousins is narrowing. (okasaki Ch 1)

For example, in C, quicksort sorts the array in place. It uses no extra storage.

By contrast, the Haskell program allocates quite a lot of extra memory behind the scenes, and runs rather slower than the C program.

	// To sort array a[] of size n: qsort(a,0,n-1)
  
	void qsort(int a[], int lo, int hi) 
 	{
	int h, l, p, t;
  
    if (lo < hi) {
      l = lo;
      h = hi;
      p = a[hi];
  
      do {
        while ((l < h) && (a[l] <= p)) 
            l = l+1;
        while ((h > l) && (a[h] >= p))
            h = h-1;
        if (l < h) {
            t = a[l];
            a[l] = a[h];
            a[h] = t;
        }
      } while (l < h);
  
      a[hi] = a[l];
      a[l] = p;
  
      qsort( a, lo, l-1 );
      qsort( a, l+1, hi );
    }
  }

  	quicksort :: Ord a => [a] -> [a]
  	quicksort []     = []
  	quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
      where
          lesser  = filter (< p) xs
          greater = filter (>= p) xs
  
In applications where performance is required at any cost, or when the goal is detailed tuning of a low-level algorithm, an imperative language like C would probably the best choice.


# The Advantages of FP are Significant

- Less debugging
- Formal provability
- Composability
- Modularity

TODO: include disclaimer that many of us are likely never going to get a job using a functional language -- learning the functional programming paradigm is more for self-improvement that practical skill-building.

# Advantages of FP

#### Less debugging


* If you can get the logic correct, functional programming requires orders of magnitude less code than imperative programming.
* That means fewer points of failure, less code to test, and a more productive (and, many would say, happier) programming life.
* As systems get bigger, this has become more and more important.

* Look at inputs and outputs to quickly isolate the source of a bug.
* No need to track down side-effects, off-by-one bugs, (as many) mistakes in statement order, and more.

# Advantages of FP

#### Formal provabiliity

*  Principles of proof by mathematical induction go hand-in-hand with the programming technique of recursion.
*  Specifically, there is a close correspondence between the mathematical reasoning that justifies the correctness of a program and the program itself.

[8](http://www.cs.cmu.edu/~15150/index.html)

# Advantages of FP

#### Modularity

"FP emphasizes the isolation of abstract types that clearly separate implementation from interface. Types are used to express and enforce abstraction boundaries, greatly enhancing maintainability of programs, and facilitating team development."

[8]

TODO Example

# Advantages of FP

#### Composability

* "Composing" functions basically just means sticking two or more functions together to make a big function that combines their functionality in a useful way.
* In functional programming, functional composition is essentially identical to functional composition in mathematics. If I have a function `f(x) = x^2` and `g(x) = x + 1`, composing the functions means creating a new function in which arguments are given to the inner function, and after the inner function computes the result, this output is given as the input to the outer function. Composing  f outer with g inner could be written `f(g(x))`. If you provide a value of 1 for `x`, then `g(1) == 1 + 1 == 2`, so `f(g(1)) == f(2) == 2^2 == 4`.
	* It's the ability to assemble  complex behavior by aggregating simpler behavior
	* Something is _composable_ when several instances can be combined in a certain way to produce the same type of thing.
	* Because functions in FP are dependent only on their input, and not any global state or side effect, subproblems can be solved independently with solving the other one.

For instance,
	
	descendingSort = (reverse . sort)
	countdown = descendingSort [2, 8, 7, 10, 1, 9, 5, 3, 4, 6]

# Other fun advantages
- Ease of understanding code at first glance
- Brevity
- Code re-use
- Powerful abstractions
- Built-in memory management

# Brevity

- Functional programs tend to be shorter by a factor of 2 to 10.

We can write the previous quicksort example even more concisely using Haskell with the help of what are called _list comprehensions_:

`qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]`


Compare this one-liner to the equivalent in C:

  // To sort array a[] of size n: qsort(a,0,n-1)
  
  void qsort(int a[], int lo, int hi) 
  {
    int h, l, p, t;
  
    if (lo < hi) {
      l = lo;
      h = hi;
      p = a[hi];
  
      do {
        while ((l < h) && (a[l] <= p)) 
            l = l+1;
        while ((h > l) && (a[h] >= p))
            h = h-1;
        if (l < h) {
            t = a[l];
            a[l] = a[h];
            a[h] = t;
        }
      } while (l < h);
  
      a[hi] = a[l];
      a[l] = p;
  
      qsort( a, lo, l-1 );
      qsort( a, l+1, hi );
    }
  }


# Homework
1. Read the first few sections in [Real World Haskell](http://book.realworldhaskell.org/read/why-functional-programming-why-haskell.html#id528893), titled "Novelty", "Power", and "Enjoyment"
2. Read [the intro and Haskell installation instructions](http://learnyouahaskell.com/introduction) _Learn You a Haskell for Great Good_

  * Come Thursday, I'll expect that you are comfortable with
    - Using GHCI 
    - Basic functions defined in the `Prelude` module
      * Hint: Search for "Prelude" in [Hoogle](http://www.haskell.org/hoogle/)
    - Lists
    - Ranges
    - List Comprehensions
    - Tuples
* Please email me or schedule an appointment with me if you want help!! We'll dive into types on Thursday.

* Optional Reading:
	* [Why Functional Programming Matters](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf)

# References

- [Okasaki](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
- [Examples of Referential Transparency](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)
- [2](http://docs.python.org/howto/functional.html)