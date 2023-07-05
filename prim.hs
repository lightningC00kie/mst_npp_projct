
import qualified Data.Map as Map
import qualified Data.Set as Set
type Vertex = Int

type Edge = (Weight, Vertex, Vertex)

type Graph = ([Vertex], [Edge])

exampleGraph :: Graph
exampleGraph = ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], edges)
  where
    edges = [ (5, 1, 2)
            , (2, 1, 3)
            , (3, 2, 4)
            , (7, 2, 5)
            , (6, 3, 6)
            , (4, 4, 5)
            , (1, 4, 7)
            , (8, 5, 8)
            , (3, 6, 9)
            , (9, 7, 10)
            ]

g2 :: Graph
g2 = ([1, 2, 3, 4, 5, 6, 7, 8], edges)
  where
    edges =
      [ (2, 1, 7)
      , (5, 1, 6)
      , (1, 7, 6)
      , (4, 6, 2)
      , (3, 2, 8)
      , (6, 6, 8)
      , (3, 6, 4)
      , (4, 6, 3)
      , (2, 3, 5)
      , (5, 6, 5)
      ]

data LHeap a = Nil | Node (LHeap a) a (LHeap a)

rank :: LHeap a -> Int
rank Nil = 0
rank (Node _ _ right) = 1 + rank right

merge :: Ord a => LHeap a -> LHeap a -> LHeap a
merge Nil h = h
merge h Nil = h
merge h@(Node left x right) h'@(Node _ x' _)
    | x' < x = merge h' h
    | otherwise =
        let h1 = merge right h'
        in if rank left >= rank h1 then Node left x h1 else Node h1 x left

insert :: Ord a => a -> LHeap a -> LHeap a
insert x h = merge (Node Nil x Nil) h

deleteMin :: Ord a => LHeap a -> (a, LHeap a)
deleteMin (Node left x right) = (x, merge left right)

findMin :: Ord a => LHeap a -> Maybe a
findMin Nil = Nothing
findMin (Node _ x _) = Just x

contains :: Ord a => a -> LHeap a -> Bool
contains _ Nil = False
contains x (Node left y right)
  | x < y = contains x left
  | x > y = contains x right
  | otherwise = True

remove :: Ord a => a -> LHeap a -> LHeap a
remove _ Nil = Nil
remove x (Node left y right)
  | x < y = Node (remove x left) y right
  | x > y = Node left y (remove x right)
  | otherwise = merge left right

heapify :: Ord a => LHeap a -> LHeap a
heapify Nil = Nil
heapify (Node left x right)
  | rank left >= rank right = Node left x (heapify right)
  | otherwise = Node (heapify right) x left

instance Show a => Show (LHeap a) where
  show Nil            = "Nil"
  show (Node l x r)   = "Node " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

type Weight = Double

neighbors:: Graph -> Vertex -> [Vertex]
neighbors (_, es) v = [u | (_, v', u) <- es, v'==v]

initializeHeap:: [Vertex] -> LHeap Edge -> LHeap Edge
initializeHeap [] lheap = lheap
initializeHeap (v:vs) Nil = initializeHeap vs firstHeap where
  firstHeap = insert (0,-1, v) Nil
initializeHeap (v:vs) lheap = initializeHeap vs newHeap
  where
    newHeap = insert (1.0/0.0, -1, v) lheap

updateKeys:: Graph -> Vertex -> LHeap Edge -> Map.Map (Vertex, Vertex) Weight -> Set.Set Vertex -> Map.Map Vertex [Vertex] -> LHeap Edge
updateKeys g v h m vis nbsMap = f v h m (Map.lookup v nbsMap) vis nbsMap
  where
    f:: Vertex -> LHeap Edge -> Map.Map (Vertex, Vertex) Weight -> Maybe [Vertex] -> Set.Set Vertex -> Map.Map Vertex [Vertex] -> LHeap Edge
    f _ heap _ Nothing _ _ = heap
    f _ heap _ (Just []) _ _ = heap
    f vertex heap wMap (Just (nbr:rest)) visited nbsMap
      | Set.member nbr visited == True = f vertex heap wMap (Just rest) visited nbsMap
      | otherwise = f vertex (updateKeysHelper vertex nbr heap wMap) wMap (Just rest) visited nbsMap

updateKeysHelper :: Vertex -> Vertex -> LHeap Edge -> Map.Map (Vertex, Vertex) Weight -> LHeap Edge
updateKeysHelper src dest heap weightsMap  = case getWeight src dest weightsMap of
  Just weight -> insert (weight, src, dest) heap
  Nothing     -> heap

neighborsMap :: Graph -> Map.Map Vertex [Vertex]
neighborsMap g@(vertices, _) = foldr addNeighbor Map.empty vertices
  where
    addNeighbor :: Vertex -> Map.Map Vertex [Vertex] -> Map.Map Vertex [Vertex]
    addNeighbor v = Map.insert v (neighbors g v)

getWeight :: Vertex -> Vertex -> Map.Map (Vertex, Vertex) Weight -> Maybe Weight
getWeight v u m = Map.lookup (v, u) m

weightMap :: Graph -> Map.Map (Vertex, Vertex) Weight
weightMap (_, edges) = foldr addWeight Map.empty edges
  where
    addWeight :: Edge -> Map.Map (Vertex, Vertex) Weight -> Map.Map (Vertex, Vertex) Weight
    addWeight (w, v, u) = Map.insert (v, u)  w

prim :: Graph -> [Edge]
prim g@(vs, _) = primHelper g (initializeHeap vs Nil) [] Set.empty (weightMap g) (neighborsMap g)

primHelper:: Graph -> LHeap Edge -> [Edge] -> Set.Set Vertex -> Map.Map (Vertex, Vertex) Weight -> Map.Map Vertex [Vertex] -> [Edge] 
primHelper g@(vs, _) heap mst vis wMap nbsMap
  | length mst == length vs = mst
  | Set.member u vis == True = primHelper g newHeap mst vis wMap nbsMap
  | otherwise = primHelper g (updateKeys g u newHeap wMap vis nbsMap) (minEdge:mst) (Set.insert u vis) wMap nbsMap
  where
    (minEdge, newHeap) = deleteMin heap
    (_, _, u) = minEdge

sumMst:: [Edge] -> Weight
sumMst [] = 0
sumMst ((w, _, _):es) = w + sumMst es