
- d3-graphviz
    - problems with overlapping edges
    - solution https://stackoverflow.com/questions/5343899/how-to-force-node-position-x-and-y-in-graphviz


Tried and failed approaches:
- cytoscape-dagre -> 
    - problem with multiedges, e.g. when having true/false both go to if-end
    - it just merges 2 edges..
    - dagre layout isnt suitable for ifs and whiles..
- flowchartjs ->
    - cant add metadata to edges, which is needed when inserting new node etc.
    - cant use direct recursion, eg. empty while loop
- mxgraph -> 
    - I forgot what was the issue, layout maybe
