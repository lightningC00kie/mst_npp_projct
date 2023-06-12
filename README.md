Non-procedural-programming semester project.

Project Description:

Finding a minimum spanning tree of a given graph
in Haskell using Prim's and Kruskal's algorithms.
The program takes inputs:
- file containing graph description
- flag indicating which algorithm to use. -p for Prim's and -k for Kruskal's

The program will output the edges of the minimum
spanning tree and the weight of the tree.

The file describing the graph has the following format:
first line is a list of integers separated by a space representing the vertices
of the graph. Each subsequent line contains 3 integers separated by a space,
where each line describes an edge. The first 2 integers are source and destination
vertices (order doesn't matter because it's an undirected graph), and the third
integer represents the weight of the edge.
