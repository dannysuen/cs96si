# Today's Topic: Adavntages of Functional Design

Now that we understand

- recursion
- lazy evaluation
- curried functions
- higher order functions
- functions as first-class entities
- lamddas
- closures

we can discuss the advantages of functional design, including

- Formal provability.
- Modularity.
- Composability.
- Ease of debugging and testing.

# Formal Provability

(from the Python site)

A theoretical benefit is that it’s easier to construct a mathematical proof that a functional program is correct.

For a long time researchers have been interested in finding ways to mathematically prove programs correct. This is different from testing a program on numerous inputs and concluding that its output is usually correct, or reading a program’s source code and concluding that the code looks right; the goal is instead a rigorous proof that a program produces the right result for all possible inputs.

The technique used to prove programs correct is to write down invariants, properties of the input data and of the program’s variables that are always true. For each line of code, you then show that if invariants X and Y are true before the line is executed, the slightly different invariants X’ and Y’ are true after the line is executed. This continues until you reach the end of the program, at which point the invariants should match the desired conditions on the program’s output.

Functional programming’s avoidance of assignments arose because assignments are difficult to handle with this technique; assignments can break invariants that were true before the assignment without producing any new invariants that can be propagated onward.

Unfortunately, proving programs correct is largely impractical and not relevant to Python software. Even trivial programs require proofs that are several pages long; the proof of correctness for a moderately complicated program would be enormous, and few or none of the programs you use daily (the Python interpreter, your XML parser, your web browser) could be proven correct. Even if you wrote down or generated a proof, there would then be the question of verifying the proof; maybe there’s an error in it, and you wrongly believe you’ve proved the program correct.

# Modularity

A more practical benefit of functional programming is that it forces you to break apart your problem into small pieces. Programs are more modular as a result. It’s easier to specify and write a small function that does one thing than a large function that performs a complicated transformation. Small functions are also easier to read and to check for errors.

# Ease of debugging and testing

Testing and debugging a functional-style program is easier.

Debugging is simplified because functions are generally small and clearly specified. When a program doesn’t work, each function is an interface point where you can check that the data are correct. You can look at the intermediate inputs and outputs to quickly isolate the function that’s responsible for a bug.

Testing is easier because each function is a potential subject for a unit test. Functions don’t depend on system state that needs to be replicated before running a test; instead you only have to synthesize the right input and then check that the output matches expectations.

# Composability

As you work on a functional-style program, you’ll write a number of functions with varying inputs and outputs. Some of these functions will be unavoidably specialized to a particular application, but others will be useful in a wide variety of programs. For example, a function that takes a directory path and returns all the XML files in the directory, or a function that takes a filename and returns its contents, can be applied to many different situations.

Over time you’ll form a personal library of utilities. Often you’ll assemble new programs by arranging existing functions in a new configuration and writing a few functions specialized for the current task.

