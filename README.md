

TODO

- clear output button
- validate return type with return expr !!!


# FlowRun

Flowcharts, runnable in your browser!

---
## Features
- simple and fast flowchart editor
- run program inside your browser, locally
- export program into JSON
- readonly mode for documentation/tutorials
- predefined functions
- precise error reporting

### Data types
- `Integer` for whole numbers
- `Real` for decimal numbers
- `String` for text
- `Boolean` for true/false

### Operators
- `+`, `-`, `*`, `/`, `%` for arithmetic operations
- `&&`, `||`, `!` for boolean operations
- `<`, `<=`, `==`, `!=`, `>`, `>=` for comparing values

The rules of precedence should be familiar to you from mathematics.  
For example:
- `*` has higher precedence than `+`:  
  `2 + 2 * 2` == `2 + 4` == `6`.
- `&&` has higher precedence than `||`:  
  `true || false && true` == `true || false` == `true`.

### Functions
You can define new functions and use them from your `main` function.  
Functions have a *return type*, the type of value which it returns.  
For example, if you calculate a sum of two `Integer`s, the result would also be an `Integer` (return type).

The return type can also be `Void`, which means it doesn't return anything, it just executes and that's it.  
For example, it could calculate something, print it and exit.

There are also some *predefined functions* that you can use, they are defined automatically in FlowRun:
- `randomInteger(top)` returns a random `x: Integer`, in the range `[0,top)`
- `abs(x)` calculates absolute value of x

---
## Implementation details
Written in [ScalaJS](https://www.scala-js.org/).  
Uses [d3-graphviz](https://github.com/magjac/d3-graphviz) to display the flowcharts.
