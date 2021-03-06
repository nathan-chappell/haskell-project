--Nathan Chappell
--Edmonds-Karp max flow with application of connectivity

import System.Environment
import qualified Data.Map as M
import Data.List

type NodeId = Int
type Edge = (NodeId, NodeId)
type Cap = Int
type FEdge = (NodeId, NodeId, Cap)
type FlowVal = Int

type AdjList = M.Map NodeId [NodeId]
type FlowProps = M.Map Edge (FlowVal, Cap)

data FlowGraph = FlowGraph {
  s :: NodeId, t :: NodeId,
  adjList :: AdjList, flowProps :: FlowProps
  } deriving (Show)
data FlowSystem = FlowSystem { graph :: FlowGraph, residual :: FlowGraph }

--Deubugging

printProps :: FlowGraph -> IO ()
printProps fg = putStr $ foldl (\acc x -> acc ++ (show x) ++ "\n") "" (M.toList $ flowProps fg)

printAdj :: FlowGraph -> IO ()
printAdj fg = putStr $ foldl (\acc x -> acc ++ (show x) ++ "\n") "" (M.toList $ adjList fg)
--

--Creating the data structures

--Adjacency list

adjAddFEdge :: FEdge -> AdjList -> AdjList
adjAddFEdge (u,v,_) adj =
  M.insertWith (\_ old -> old) v [] $
  M.insertWith (\[new] old -> new:old) u [v] adj

adjAddFEdges :: [FEdge] -> AdjList -> AdjList
adjAddFEdges fedges adj = foldr adjAddFEdge adj fedges

adjFromFEdges :: [FEdge] -> AdjList
adjFromFEdges fedges = adjAddFEdges fedges (M.empty :: AdjList)

fPropsFromFEdges :: [(NodeId,NodeId,Cap)] -> FlowProps
fPropsFromFEdges fedges =
  foldl (\fprops (u,v,c) -> M.insert (u,v) (0,c) fprops)
  (M.empty :: FlowProps) fedges

--Creating a residual graph from a flow graph

getResidualFEdge :: (Edge,(FlowVal,Cap)) -> [(NodeId,NodeId,Int)]
getResidualFEdge ((u,v),(f,c))
  | 0 == f && f == c  = []
  | 0 == f && f < c   = [(u,v,c)]
  | 0 < f  && f < c   = [(u,v,c-f),(v,u,f)]
  | 0 < f  && f == c  = [(v,u,f)]
  | otherwise         = error "unknown edge thingee"

residualAdj :: FlowProps -> AdjList
residualAdj flowProps = adjFromFEdges $
  foldl (\acc item -> (getResidualFEdge item) ++ acc) [] (M.toList flowProps)

vertices :: AdjList -> [NodeId]
vertices adj = foldl union [] (map (\(n, ns) -> n:ns) $ M.toList adj)


--Shortest Path algorithm for Edmonds-Karp

type ShortestPathUpdateAcc = ([NodeId], M.Map NodeId NodeId, AdjList)

shortestPathUpdate :: ShortestPathUpdateAcc -> NodeId -> ShortestPathUpdateAcc
shortestPathUpdate (q, prevMap, g) u =
  let neighbours = filter (\x -> not $ M.member x prevMap) $ g M.! u in
    foldl (\(q',prevMap', g) v -> (v:q', M.insert v u prevMap', g))
    (q, prevMap, g) neighbours

getPath :: NodeId -> M.Map NodeId NodeId -> [Edge]
getPath (-10000000) prevMap = []
getPath t prevMap = (prev,t) : (getPath prev prevMap)
  where prev = prevMap M.! t 

shortestPath' :: NodeId -> ShortestPathUpdateAcc -> [Edge]
shortestPath' _ ([],_,_) = []
shortestPath' t (q, prevMap, g)
  | (elem t q) = reverse $ getPath t prevMap
  | otherwise = shortestPath' t (foldl shortestPathUpdate ([],prevMap,g) q)

shortestPath :: NodeId -> NodeId -> AdjList -> [Edge]
shortestPath s t adj =
  filter (\(u,v) -> u /= (-10000000)) $ shortestPath' t ([s], M.singleton s (-10000000), adj)

--Epsilon is the largest allowable increment on the shortest path

getEpsilon :: [Edge] -> FlowProps -> Int
getEpsilon edges props =
  minimum $ map (\(x,y) -> y-x) $ map (\x -> props M.! x) edges

{-
The algorithm expects that no lenght-2 cycles exist in the flow network.  These cycles are removed
with the "formatting" functions below, by the method shown in this drawing:

  -->     becomes:  a --> a*10000+b --> b
a <-- b                     <----
-}

formatMaxFlow :: [FEdge] -> [FEdge]
formatMaxFlow fedges =
  foldl (\acc (u,v,c) -> if (elem (v,u) $ map (\(u',v',_) -> (u',v')) acc)
                         then (u,v+u*10000,c):(v+u*10000,v,c):acc else (u,v,c):acc)
  [] fedges

--last stage of preparation...
--formatMaxConn :: [FEdge] -> [FEdge]
--formatMaxConn fedges = 

deformatFolder :: [(Edge,(Int,Int))] -> (Edge,(Int,Int)) -> [(Edge,(Int,Int))]
deformatFolder acc ((u,v),(f,c))
  | v >= 10000 = ((u,v `mod` 10000),(f,c)):acc
  | u >= 10000 = acc
  | otherwise = ((u,v),(f,c)):acc

deformatFlow :: FlowGraph -> FlowGraph
deformatFlow (FlowGraph s t adj flow) = FlowGraph s t adj newFlow
  where newFlow = M.fromList $ foldl deformatFolder [] (M.toList flow)

--Edmonds-Karp algorithm

updateFlow :: [Edge] -> FlowGraph -> FlowGraph
updateFlow sPath (FlowGraph s t adj flow) = FlowGraph s t adj flow'
  where epsilon = getEpsilon sPath flow
        flow' = foldl (\flow'' e ->
                         M.insertWith (\(ep,_) (f,c) -> (f+ep,c))
                         e (epsilon,0) flow'')
                flow sPath

maximizeFlow :: FlowGraph -> FlowGraph
maximizeFlow (FlowGraph s t adj flow) 
  | null sPath = (FlowGraph s t adj flow)
  | otherwise       = maximizeFlow updatedFlow
  where sPath       = shortestPath s t $ residualAdj flow
        updatedFlow = updateFlow sPath (FlowGraph s t adj flow)

--String/ IO manipulation for the algorithm

readFEdges :: String -> [FEdge]
readFEdges str =
  map (\[u,v,c] -> (u,v,c)) (map (map read) $ map words $ lines str :: [[NodeId]])

getFEdgesFromFile :: String -> IO ([FEdge])
getFEdgesFromFile fileName = do
  text <- readFile fileName
  return $ readFEdges text

readFEdgesFromLines :: [String] -> [FEdge]
readFEdgesFromLines lines =
  map (\[u,v,c] -> (u,v,c)) (map (map read) $ map words lines)

getST :: String -> [NodeId]
getST line = map read $ words line

flowGraphFromText :: String -> FlowGraph
flowGraphFromText text = 
    FlowGraph s t (adjFromFEdges fEdges) (fPropsFromFEdges fEdges)
    where fLines = lines text
          [s,t] = getST $ head fLines
          fEdges = readFEdgesFromLines $ tail fLines

prettyPrintFlow :: FlowGraph -> IO ()
prettyPrintFlow (FlowGraph s t adj flow) =
  putStrLn $ "total flow: " ++ show (totalFlow (FlowGraph s t adj flow)) ++ "\n" ++ 
  concat (map (\((u,v),(f,c)) -> show (u,v) ++ " " ++
                show f ++ "/" ++ show c ++ "\n") (M.toList flow))

maximizeFromFile :: String -> IO ()
maximizeFromFile fileName = do
  text <- readFile fileName
  prettyPrintFlow . maximizeFlow $ flowGraphFromText text

{-
Connectivity Algorithm.
Maximum connectivity is determined by taking a list of edges corresponding to an undirected graph,
and tranforming the graph into a flow network in which only 1-unit of flow may pass through any node,
determining the max flow between all pairs of vertices, and taking the minimum determined max-flow.
This is very inefficient, on the order of |E|^8.
-}

--Reports the total flow from a flowgraph

totalFlow :: FlowGraph -> Int
totalFlow (FlowGraph s t adj flow) = outs -- - ins
  where outs = foldl (\acc (_,(f,_)) -> acc + f) 0 (filter (\((u,v),_) -> u == s) (M.toList flow))
        ins = foldl (\acc (_,(f,_)) -> acc + f) 0 (filter (\((u,v),_) -> v == s) (M.toList flow))

--Making the data structures for the algorithm


fedgesForConn ::[Edge] -> [FEdge]
fedgesForConn edges = nub $ 
  foldl (\acc (x,y) -> (x,y,1):acc) [] $ 
    foldl (\acc (x,y) -> (x,(-1)*y):(y,(-1)*x):((-1)*x,x):((-1)*y,y):acc) [] edges

flowGraphForConn :: NodeId -> NodeId -> [FEdge] -> FlowGraph
flowGraphForConn s t fedges = FlowGraph s t (adjFromFEdges fedges) (fPropsFromFEdges fedges)

connectivityTest' :: NodeId -> NodeId -> [FEdge] -> Int
connectivityTest' s t fedges = totalFlow $ maximizeFlow $ flowGraphForConn s t fedges


connectivityTest :: [Edge] -> Int
connectivityTest edges = minimum flows
  where fedges = fedgesForConn edges
        tests = nub $ foldl (\acc (x,y) -> (x,(-1)*y):(y,(-1)*x):acc) [] edges
        flows = foldl (\acc (s,t) -> (connectivityTest' s t fedges):acc) [] tests


edgesFromText :: String -> [Edge]
edgesFromText text =
  map (\[us, vs] -> (read us, read vs) :: (Int,Int)) $ map words $ lines text

ctestFromFile :: String -> IO ()
ctestFromFile filename = do
  text <- readFile filename
  putStrLn $ show $ connectivityTest $ edgesFromText text

--
  
main = do
  args <- getArgs
  let opt = head args
      fname = head $ tail args in
    case opt of 
               "-f" -> maximizeFromFile fname
               "-c" -> ctestFromFile fname
               _ -> putStrLn "usage:\n -f filename for flow analysis\n -c filename for connectivity test"

