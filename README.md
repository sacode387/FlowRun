# FlowRun

Flowcharts, runnable in your browser!

---
## Features
- simple and fast flowchart editor
- runs your program inside your browser, locally
- export program into JSON
- show mode, no editor included, nice for documentation/tutorials
- predefined constants and functions

### Data types
- `Integer` for whole numbers
- `Real` for decimal numbers
- `String` for text
- `Boolean` for true/false

### Functions
You can define new functions and use them from your `main` function.  
Functions have a *return type*, the type of value which it returns.  
For example, if you calculate a sum of two `Integer`s, the result would also be an `Integer` (return type).


For some functions the return type can be `Void`, which means it doesn't return anything, it just executes and that's it.  
It could calculate something, print it and just exit.

There are also some *predefined functions*, defined automatically in FlowRun:
- `randomInteger(top)` returns a random `x: Integer`, in the range `[0,top)`
- `abs(x)` calculates absolute value of x

---
## Implementation details
Written in [ScalaJS](https://www.scala-js.org/).  
Uses [CytoscapeJS](https://js.cytoscape.org/) to display and edit the flowcharts.
