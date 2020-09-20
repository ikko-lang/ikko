module Util.Graph (
  Graph,
  components,
  nodes,
  children,
  reachable,
  pathExists,
  nodesInCycle,
  ) where

import Control.Monad (when)
import Control.Monad.State (State, modify, gets, evalState)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph a = Map a [a]

nodes :: Graph a -> [a]
nodes = Map.keys

-- This is pretty inefficient
--- this could have just used strongly connected compnents!
nodesInCycle :: (Ord a) => Graph a -> [a]
nodesInCycle graph =
  let startingReachableSets = Map.map Set.fromList graph
      asList = Map.toList graph
      queue = [(n, c) | (n, cs) <- asList, c <- cs]
      reachableSets = Map.toList $ findReachableSets graph startingReachableSets queue
  in [node | (node, reachableSet) <- reachableSets, Set.member node reachableSet]

findReachableSets :: (Ord a) => Graph a -> Map a (Set a) -> [(a, a)] -> Map a (Set a)
findReachableSets _     reachableSets []               = reachableSets
findReachableSets graph reachableSets ((from,to):rest) =
  let getReachable node = fromMaybe Set.empty (Map.lookup node reachableSets)
      currentReachable = getReachable from
      nextReachable = getReachable to
      newReachable = Set.union currentReachable nextReachable
      reachableSets' = Map.insert from newReachable reachableSets
      addedReachable = Set.difference nextReachable currentReachable
      newQueueItems = [(from, to') | to' <- Set.toList addedReachable]
  in findReachableSets graph reachableSets' (newQueueItems ++ rest)

children :: (Ord a) => Graph a -> a -> [a]
children graph node =
  fromMaybe [] (Map.lookup node graph)


reachable :: (Ord a) => a -> Graph a -> Set a
reachable node graph = findReachable (children graph node) (Set.singleton node)
  where findReachable []     seen = seen
        findReachable (c:cs) seen =
          if Set.member c seen
          then findReachable cs seen
          else findReachable (children graph c ++ cs) (Set.insert c seen)

pathExists :: (Ord a) => Graph a -> a -> a -> Bool
pathExists graph start end = pathExists' start Set.empty
  where findPath nextNodes seen =
          any (`pathExists'` seen) nextNodes
        pathExists' current seen =
          let currentSeen  = Set.member current seen
              nextNodes    = children graph current
              endReachable = end `elem` nextNodes
              hasPath      = findPath nextNodes (Set.insert current seen)
          in endReachable || (not currentSeen && hasPath)

-- This finds strongly-connected components of the graph,
-- and returns them in a topological ordering.
-- This assumes that every node is mentioned as a key in the graph.
-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
components :: (Ord a) => Graph a -> [[a]]
components graph = evalState runConnect startingSCCState
  where startingSCCState = SCCState
                           { sccIndex = 0
                           , sccLowlinks = Map.empty
                           , sccIndexes = Map.empty
                           , sccComponents = []
                           , sccStack = []
                           , sccInStack = Set.empty }
        runConnect = connect (nodes graph) graph

data SCCState a
  = SCCState
    { sccIndex      :: Int
    , sccLowlinks   :: Map a Int
    , sccIndexes    :: Map a Int
    , sccComponents :: [[a]]
    , sccStack      :: [a]
    , sccInStack    :: Set a
    }

connect :: (Ord a) => [a] -> Graph a -> State (SCCState a) [[a]]
connect [] _ =
  gets sccComponents
connect (n:ns) graph = do
  exists <- isIndexed n
  if exists
    then connect ns graph
    else do
      strongConnect n graph
      connect ns graph

strongConnect :: (Ord a) => a -> Graph a -> State (SCCState a) ()
strongConnect node graph = do
  idx <- gets sccIndex
  modify $ setIndex node idx
  modify $ setLowlink node idx
  modify incIndex
  modify $ pushStack node

  connectChildren (children graph node) node graph
  gatherComponent node

connectChildren :: (Ord a) => [a] -> a -> Graph a -> State (SCCState a) ()
connectChildren []     _    _     = return ()
connectChildren (c:cs) node graph = do
  exists <- isIndexed c
  isOnStack <- gets (Set.member c . sccInStack)
  if not exists
    then do
      strongConnect c graph
      vll <- gets (getLowlink node)
      cll <- gets (getLowlink c)
      modify $ setLowlink node (min vll cll)
    else when isOnStack $ do
           vll <- gets (getLowlink node)
           cidx <- gets (getIndex c)
           modify $ setLowlink node (min vll cidx)
  connectChildren cs node graph

gatherComponent :: (Ord a) => a -> State (SCCState a) ()
gatherComponent node = do
  index <- gets (Map.lookup node . sccIndexes)
  lowlink <- gets (Map.lookup node . sccLowlinks)
  when (lowlink == index) $ do
    component <- popComponent node
    modify $ \sccs -> sccs { sccComponents = component : sccComponents sccs }

popComponent :: (Ord a) => a -> State (SCCState a) [a]
popComponent node = do
  w <- popStack
  if w == node
    then return [w]
    else do
      rest <- popComponent node
      return $ w : rest

type SCCStateTX a = SCCState a -> SCCState a

isIndexed :: (Ord a) => a -> State (SCCState a) Bool
isIndexed node = do
  indexes <- gets sccIndexes
  return $ Map.member node indexes

pushStack :: (Ord a) => a -> SCCStateTX a
pushStack node sccs =
  sccs { sccStack = node : sccStack sccs
       , sccInStack = Set.insert node (sccInStack sccs) }

popStack :: (Ord a) => State (SCCState a) a
popStack = do
  stack <- gets sccStack
  let item = head stack
  modify $ \scss ->
    scss { sccStack = tail stack
         , sccInStack = Set.delete item (sccInStack scss) }
  return item

incIndex :: SCCStateTX a
incIndex sccs = sccs { sccIndex = 1 + sccIndex sccs }

setIndex :: (Ord a) => a -> Int -> SCCStateTX a
setIndex node index sccs =
  sccs { sccIndexes = Map.insert node index (sccIndexes sccs) }

setLowlink :: (Ord a) => a -> Int -> SCCStateTX a
setLowlink node lowlink sccs =
  sccs { sccLowlinks = Map.insert node lowlink (sccLowlinks sccs) }

getIndex :: (Ord a) => a -> SCCState a -> Int
getIndex node sccs = fromJust $ Map.lookup node $ sccIndexes sccs

getLowlink :: (Ord a) => a -> SCCState a -> Int
getLowlink node sccs = fromJust $ Map.lookup node $ sccLowlinks sccs

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "unexpected Nothing"
