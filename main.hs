import Data.List
import System.IO
import Control.Monad

type Vertex = Integer

type Edge = ([Vertex], Integer)

type Graph = ([Vertex], [Edge])

v1 =  1
v2 =  2
v3 =  3
v4 =  4

e1 = ([v1, v2], 2)
e2 = ([v2, v3], 1)
e3 = ([v3, v4], 1)
e4 = ([v4, v1], 4)

g = ([v1, v2, v3, v4], [e1, e2, e3, e4])

readGraphFromFile :: FilePath -> IO Graph
readGraphFromFile filePath = do
  contents <- readFile filePath
  let linesList = lines contents
      vertices = map read $ words (head linesList) :: [Vertex]
      edges = map parseEdge (tail linesList)
  return (vertices, edges)

-- read edges line by line from file and parse it
parseEdge :: String -> Edge
parseEdge line =
  let [source, destination, weight] = map read $ words line :: [Vertex]
  in ([source, destination], weight)

-- number of vertices in a graph
numVertices :: Graph -> Int
numVertices (vertices, _) = length vertices

-- number of edges in a graph
numEdges :: Graph -> Int
numEdges (_, edges) = length edges

-- true if a vertex is part of an edge
vertexInEdge :: Edge -> Vertex -> Bool
vertexInEdge (vertices, _) vertex = elem vertex vertices

-- given a list of edges, return a list of vertices that
-- are incident with any edge in the list of edges
getVerticesFromEdges :: [Edge] -> [Vertex]
getVerticesFromEdges edges = nub (concat [vs | (vs, _) <- edges])

-- returns all edges that are incident with a given vertex
incidentEdges :: [Edge] -> Vertex -> [Edge]
incidentEdges edges vertex = [e | e <- edges , vertexInEdge e vertex]

-- true if the list of edges contains a cycle
isCycle :: [Edge] -> Bool
isCycle edges = length edges >= length(getVerticesFromEdges edges)

-- returns all edges that are in the same forest
getForest :: [Edge] -> [Vertex] -> [Edge]
getForest es ns
  | length vs == length ns = incidents
  | otherwise = getForest es vs
  where
    incidents = nub (concatMap (incidentEdges es) ns)
    vs = getVerticesFromEdges incidents

-- true if adding the edge to the given list
checkForCycle :: [Edge] -> Edge -> Bool
checkForCycle edges (edgeVertices,_) = isCycle cnEs
    where cnEs = getForest edges edgeVertices

addEdge :: Graph -> Graph -> Graph
addEdge g@(vertices1, edges1) mst@(vertices2, edges2) =
  let edges = sortEdges (edges1 \\ edges2)
  in addEdgeHelper edges mst

sortEdges :: [Edge] -> [Edge]
sortEdges edges = sortBy (\(_,w1) (_,w2) -> compare w1 w2) edges

addEdgeHelper :: [Edge] -> Graph -> Graph
addEdgeHelper (e@(edgeVertices, _):edges) g@(graphVertices, es)
  | cyclesWithEdge = addEdgeHelper edges g
  | otherwise = g'
  where
    es' = e:es
    g' = (graphVertices, es')
    cyclesWithEdge = isCycle (getForest es' edgeVertices)

kruskal :: Graph -> Graph
kruskal g@(vertices, edges) = kruskalHelper g (vertices,[])

-- base case is when the number of vertices is one more than the number of edges (definition of a tree)
-- recursive case: we add an edge to the mst that wasn't already in it and won't cause a cycle
kruskalHelper :: Graph -> Graph -> Graph
kruskalHelper g mst
  | numVertices mst == 1 + numEdges mst = mst
  | otherwise =
      let mst' = addEdge g mst
      in kruskalHelper g mst'

main :: IO ()
main = do
  graph <- readGraphFromFile "graph.txt"
  -- Handle the obtained graph here
  let mst = kruskal graph -- Just an example, you can perform any operations on the graph
  print mst