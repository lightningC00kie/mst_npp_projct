import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)
import System.Environment (getArgs)

type Vertex = Int
type Edge = (Weight, Vertex, Vertex)
type Weight = Double
type UnionFind a = Map.Map a a
type Graph = ([Vertex], [Edge])

-- e1:: [Edge]
-- e1 = [ (4.0, 1, 2)
--     , (8.0, 1, 8)
--     , (8.0, 2, 3)
--     , (11.0, 2, 8)
--     , (7.0, 3, 4)
--     , (4.0, 3, 6)
--     , (2.0, 3, 9)
--     , (9.0, 4, 5)
--     , (14.0, 4, 6)
--     , (10.0, 5, 6)
--     , (2.0, 6, 7)
--     , (1.0, 7, 8)
--     , (6.0, 7, 9)
--     , (7.0, 8, 9)
--     ]

-- g1:: Graph
-- g1 = ([1, 2, 3, 4, 5, 6, 7, 8, 9], e1)
        

hasCycle :: [Edge] -> Bool
hasCycle es = hasCycleFrom es Map.empty

hasCycleFrom :: [Edge] -> UnionFind Vertex -> Bool
hasCycleFrom [] _ = False
hasCycleFrom ((_, src, dst) : rest) uf =
  let rootSrc = find src uf
      rootDst = find dst uf
  in if rootSrc /= rootDst
       then hasCycleFrom rest (union rootSrc rootDst uf)
       else True

unionFindRank :: Vertex -> UnionFind Vertex -> Int -> Int
unionFindRank v uf counter =
  case Map.lookup v uf of
    Just parent | parent /= v -> unionFindRank parent uf (counter + 1)
    _ -> counter

find :: Vertex -> UnionFind Vertex -> Vertex
find x uf =
  case Map.lookup x uf of
    Just parent | parent /= x -> find parent uf
    _ -> x

union :: Vertex -> Vertex -> UnionFind Vertex -> UnionFind Vertex
union x y uf
  | rootX == rootY = uf
  | (unionFindRank rootX uf 0) < (unionFindRank rootY uf 0) = Map.insert rootX rootY uf
  | otherwise = Map.insert rootY rootX uf 
  where
    rootX = find x uf
    rootY = find y uf

sortByWeight :: [Edge] -> [Edge]
sortByWeight = sortBy (compare `on` (\(w, _, _) -> w))

kruskal:: Graph -> [Edge]
kruskal (vs,es) = kruskalHelper (vs, (sortByWeight es)) []

kruskalHelper:: Graph -> [Edge] -> [Edge]
kruskalHelper ([], _) _ = []
kruskalHelper (_, []) [] = []
kruskalHelper (_, []) mst = mst
kruskalHelper (vs, (e:es)) mst
  | length mst == (length vs) - 1 = mst
  | hasCycle (e:mst) == True = kruskalHelper (vs, es) mst
  | otherwise = kruskalHelper (vs, es) (e:mst)

sumMst:: [Edge] -> Weight
sumMst [] = 0
sumMst ((w, _, _):rest) = w + sumMst rest

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

instance Show a => Show (LHeap a) where
  show Nil            = "Nil"
  show (Node l x r)   = "Node " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

neighbors:: Graph -> Vertex -> [Vertex]
neighbors (_, es) v = [u | (_, v', u) <- es, v'==v] ++ [u | (_, u, v') <- es, v'==v]

initializeHeap:: [Vertex] -> LHeap Edge -> LHeap Edge
initializeHeap [] lheap = lheap
initializeHeap (v:vs) Nil = initializeHeap vs firstHeap where
  firstHeap = insert (0,-1, v) Nil
initializeHeap (v:vs) lheap = initializeHeap vs newHeap
  where
    newHeap = insert (1.0/0.0, -1, v) lheap

updateKeysHelper :: Vertex -> Vertex -> LHeap Edge -> Map.Map (Vertex, Vertex) Weight -> LHeap Edge
updateKeysHelper src dest heap weightsMap  = case 
  (getWeight src dest weightsMap, getWeight dest src weightsMap) of
  (Just w1, Just w2) -> insert (w2, dest, src) (insert (w1, src, dest) heap)
  (Just weight, Nothing)-> insert (weight, src, dest) heap
  (Nothing, Just weight) -> insert (weight, dest, src) heap
  (Nothing, Nothing)     -> heap

updateKeys:: Graph -> Vertex -> LHeap Edge -> Map.Map (Vertex, Vertex) Weight -> Set.Set Vertex -> 
  Map.Map Vertex [Vertex] -> LHeap Edge
updateKeys g v h m vis nbsMap = f v h m (Map.lookup v nbsMap) vis 
  where
    f:: Vertex -> LHeap Edge -> Map.Map (Vertex, Vertex) Weight -> Maybe [Vertex] -> 
      Set.Set Vertex -> LHeap Edge
    f _ heap _ Nothing _  = heap
    f _ heap _ (Just []) _  = heap
    f vertex heap wMap (Just (nbr:rest)) visited 
      | Set.member nbr visited = f vertex heap wMap (Just rest) visited 
      | otherwise = f vertex (updateKeysHelper vertex nbr heap wMap) wMap (Just rest) visited 

neighborsMap :: Graph -> Map.Map Vertex [Vertex]
neighborsMap g@(vs, _) = Map.fromList [(v, neighbors g v) | v <- vs]

getWeight :: Vertex -> Vertex -> Map.Map (Vertex, Vertex) Weight -> Maybe Weight
getWeight v u m = Map.lookup (v, u) m

weightMap :: Graph -> Map.Map (Vertex, Vertex) Weight
weightMap (_, edges) = Map.fromList [((v, u), w) | (w, v, u) <- edges]

primHelper:: Graph -> LHeap Edge -> [Edge] -> Set.Set Vertex -> Map.Map (Vertex, Vertex) Weight -> 
  Map.Map Vertex [Vertex] -> [Edge] 
primHelper g@(vs, _) heap mst vis wMap nbsMap
  | length mst == length vs = mst
  | Set.notMember u vis = primHelper g (updateKeys g u newHeap wMap vis nbsMap) (minEdge:mst) 
  (Set.insert u vis) wMap nbsMap
  | Set.notMember v vis = primHelper g (updateKeys g v newHeap wMap vis nbsMap) (minEdge:mst) 
  (Set.insert v vis) wMap nbsMap
  | otherwise = primHelper g newHeap mst vis wMap nbsMap
  where
    (minEdge, newHeap) = deleteMin heap
    (_, v, u) = minEdge

prim :: Graph -> [Edge]
prim g@(vs, _) = init (primHelper g (initializeHeap vs Nil) [] Set.empty (weightMap g) (neighborsMap g))

parseEdge :: String -> Maybe Edge
parseEdge line =
  case mapMaybe readMaybe $ words line of
    [weight, source, destination] -> Just (weight, round source, round destination)
    _ -> Nothing
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

parseGraph :: [String] -> Maybe Graph
parseGraph (verticesLine : edgeLines) = do
  let vertices = map read $ words verticesLine :: [Vertex]
  edges <- traverse parseEdge edgeLines
  return (vertices, edges)
parseGraph _ = Nothing

readGraph :: FilePath -> IO (Maybe Graph)
readGraph path = do
  contents <- readFile path
  return (parseGraph (lines contents))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, flag] -> do
      maybeGraph <- readGraph filePath
      case maybeGraph of
        Just graph -> case flag of
          "-p" -> do
            let mst = prim graph
                mstWeight = sumMst mst
            putStrLn "Minimum Spanning Tree:"
            print mst
            putStrLn "MST Weight:"
            print mstWeight
          "-k" -> do
            let mst = kruskal graph
                mstWeight = sumMst mst
            putStrLn "Minimum Spanning Tree:"
            print mst
            putStrLn "MST Weight:"
            print mstWeight
          _ -> do
            putStrLn "Usage: ./main <graph-file> [-p | -k]"
            putStrLn "  -p: use Prim's algorithm"
            putStrLn "  -k: use Kruskal's algorithm"
            exitFailure
        Nothing -> do
          putStrLn "Failed to parse the graph."
          exitFailure
    _ -> do
      putStrLn "Usage: ./main <graph-file> <flag>"
      exitFailure