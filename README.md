## Minimum Spanning Tree Program

The Minimum Spanning Tree Program is a command-line tool that allows you to find the minimum spanning tree of a graph defined in a text file. It supports two algorithms: Prim's algorithm and Kruskal's algorithm.

### Prerequisites

To run the Minimum Spanning Tree Program, you need to have GHC (Glasgow Haskell Compiler) installed on your system.

### Installation

1. Clone the repository or download the source code for the Minimum Spanning Tree Program.

2. Open a terminal and navigate to the project directory.

3. Build the program by running the following command:
   ```
   ghc --make -o main Main.hs
   ```

### Usage

The Minimum Spanning Tree Program accepts a graph file as input and an optional flag to specify the algorithm. The graph file should be a text file containing the graph's vertices and edge definitions.

To run the program, use the following command format:
```
./main graphFile -flag
```

#### Arguments:

- `graphFile`: The path to the text file containing the graph's vertices and edges.

#### Flags:

- `-p`: Use Prim's algorithm to find the minimum spanning tree.
- `-k`: Use Kruskal's algorithm to find the minimum spanning tree.

### Example Usage:

Assume you have a graph defined in a file named "graph.txt". To find the minimum spanning tree using Prim's algorithm, run the following command:
```
./main graph.txt -p
```

To find the minimum spanning tree using Kruskal's algorithm, run the following command:
```
./main graph.txt -k
```

### Graph File Format:

The graph file should have the following format:

- The first line contains a space-separated list of vertices.
- Each subsequent line contains an edge definition in the format `weight vertex vertex`. The weight is a decimal value, and the vertices are integers.

Here's an example of a graph file:
```
1 2 3 4 5
1.0 1 2
2.0 1 3
3.0 2 3
4.0 2 4
5.0 3 4
```

### Output:

The program will output the minimum spanning tree as a list of edges and the weight of the minimum spanning tree.

---

Feel free to modify and customize this documentation to suit your specific program and requirements.