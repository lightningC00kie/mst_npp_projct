import Data.List
import Data.Map

v1 = Vertex "A"
v2 = Vertex "B"
v3 = Vertex "C"
v4 = Vertex "D"

e1 = Edge v1 v2 2
e2 = Edge v2 v3 1
e3 = Edge v3 v4 1
e4 = Edge v4 v1 4

g = Graph [v1,v2,v3,v4] [e1,e2,e3]

data Vertex a = Vertex a deriving (Show, Eq)

data Edge a = Edge (Vertex a) (Vertex a) Int deriving (Show, Eq)

data Graph a = Graph [Vertex a] [Edge a] deriving (Show, Eq)

getVertices :: Graph a -> [Vertex a]
getVertices (Graph vertices _) = vertices

getEdges :: Graph a -> [Edge a]
getEdges (Graph _ edges) = edges

-- isCycle :: (Eq a) => [Edge a] -> Edge a -> Bool
-- isCycle g e
--   | elem e edges = False
--   | otherwise = elem v1 vertices && elem v2 vertices 
--   where 
--     vertices = getVertices g
--     edges = getEdges g
--     (Edge v1 v2 _) = e

sortEdgesByWeight :: [Edge a] -> [Edge a]
sortEdgesByWeight = sortBy compareWeight
  where compareWeight (Edge _ _ w1) (Edge _ _ w2) = compare w1 w2


-- getUnion :: [(Vertex a, [Vertex a])] 

kruskal :: [Edge a] -> [Edge a] -> Int -> ([Edge a], Int)
kruskal (edge:edges) mst_edges total_weight
  | length mst_edges == 0 = (mst_edges, total_weight)
  | isCycle mst_edges edge == True = kruskal edges mst_edges total_weight
  | otherwise = kruskal edges (edge:mst_edges) (total_weight + weight)
  where
    (Edge _ _ weight) = edge