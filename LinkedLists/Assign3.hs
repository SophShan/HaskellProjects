{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
newtype Graph a = Graph [(Node a,[NodeID])]
  deriving (Show,Eq)

type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph = Graph [(nodeA,[1,2])
                ,(nodeB,[])
                ,(nodeC,[1,2])]




{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
 -    This function takes in an input of type Graph a and returns the largest NodeID in the graph as type Maybe NodeID. If the input given is an empty 
 Graph, the function returns Nothing. Otherwise, the first NodeID in the Graph (NodeId of the head of the Graph) and the remaining elements of the Graph 
 are passed as parameters to the function maxAux, which returns the NodeID with the maximum value.
 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID  (Graph []) = Nothing
maxNodeID  (Graph xs) = 
     let y = Just (getNodeID (fst (head xs)))
     in maxAux (Graph (tail xs)) y


{-  -----------------------------------------------------------------
 - maxAux
 - -----------------------------------------------------------------
 - Description: 
 -     This function takes an input of type Graph a and a MaybeNodeID and returns the NodeID of the node with the maximum value (of all the Nodes in the 
 Graph as well as n) as a Maybe NodeID. If the input of the Graph type is an empty Graph, it returns n as the max value as it is the only value to compare. Else, it compares n to the value of the head of the Graph. If the value of the head of the graph is greater, maxAux is called again with the tail of the graph and the NodeID of the head as n. Otherwise, maxAux is called again with the same input n and the tail of the graph.

-}
maxAux :: Graph a -> Maybe NodeID -> Maybe NodeID
maxAux (Graph []) n = n
maxAux (Graph xs) n  
  | headNodeID > n = maxAux (Graph (tail xs)) (headNodeID)
  | otherwise = maxAux (Graph (tail xs)) n
  where
     headNodeID = Just (getNodeID (fst (head xs)))
  
{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
 -    This function takes in an input of any type a as the value for the to be inserted node, and type Graph a. This function inserts a new Node to the 
 Graph given, with its value as the value inputed (v). The NodeID of the new Node is one greater than the greatest NodeID value in the inputted Graph 
 unless the Graph is empty, in which case its NodeID is 0. This function calls on maxNodeID to get the maximum NodeID of the Graph (when it is not empty), and since the output of maxNodeID is Maybe type, and type a was needed to concatenate the new node to the rest of the list of Nodes, the output was passed to the function auxMaybeConvert to convert it from Maybe type to type a.
 -}
insertNode :: a -> Graph a -> Graph a
insertNode v (Graph []) = Graph [(Node 0 v,[])]
insertNode v (Graph xs) = 
  let maxNodeIDvalue = auxMaybeConvert (maxNodeID (Graph xs))
  in Graph (xs ++ [(Node (maxNodeIDvalue + 1) v, [])])

{- -----------------------------------------------------------------
 - auxMaybeConvert
 - -----------------------------------------------------------------
 - Description:
 -    This function takes in a type Maybe int and removes its "wrapper" Just, returning a value of type a that can then be used in the function insertNode.
 -}
auxMaybeConvert :: Maybe a -> a
auxMaybeConvert (Just a) = a

{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
 -    This function takes as input a NodeID and type Graph a, removes the Node from the graph with the matching NodeID, as well as its NodeID from any of 
 the edges of the Graph, and returns a new Graph of type Graph a. If the Graph inputed is empty, the function returns an empty Graph. Else, the function 
 calls on an auxilary function auxRemove to preform the previously mentioned operations and returns the result of the auxRemove as a Graph. 
 -}
removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph []) = Graph []
removeNode nID (Graph xs) = Graph (auxRemove nID (Graph xs))
  
{- -----------------------------------------------------------------
 - auxRemove
 - -----------------------------------------------------------------
 - Description:
 -    This function takes as input a NodeId and Graph a. The function returns type [(Node a, [NodeID])] which is the same type as a Node, but without the 
 "wrapper" to label it as a Graph. If the Graph inputted is empty, the function returns an empty list. Otherwise, it check is see if the NodeID of the 
 first Node in the Graph {variable nodeID which = getNodeID (fst (head xs))} matches with the given NodeID. If it does, it skips over that element but 
 continues to traverse through the rest of the Graph nodes to remove all edges of the same NodeID. If the NodeId do not match, it returns the first node of
 the graph with any edges containing the inputted NodeId removed and combines it with another call of the function auxRemove on the tail of the Graph.
 -}
auxRemove :: NodeID -> Graph a -> [(Node a, [NodeID])]
auxRemove nID (Graph []) = []
auxRemove nID (Graph xs) 
  | nID == nodeID = auxRemove nID (Graph (tail xs)) 
  | otherwise = (Node nodeID nodeVal, auxDelEdge nID nodeList) : (auxRemove nID (Graph (tail xs))) 
   where 
      nodeID = getNodeID (fst (head xs))
      nodeVal = getNodeVal (fst (head xs))
      nodeList = snd (head xs)

{- -----------------------------------------------------------------
 - auxDelEdge
 - -----------------------------------------------------------------
 - Description:
 -    This function takes in a NodeID (int) and a list of NodeID ([int]) and returns the list of NodeId with the given NodeId removed. If an empty list is 
 imputted, an empty list will be returned. Else, if the head of the list matches the nodeID given (nID), then the remaining list is returned. Otherwise if 
 the head of the list does not match, the head is concatenated with another call of the function auxDelEdge (with the same NodeID and the tail of the list 
 as input) and is returned.
 -}
auxDelEdge :: NodeID -> [NodeID] -> [NodeID]
auxDelEdge nID [] = []
auxDelEdge nID xs 
  | head xs == nID = tail xs
  | otherwise = head xs : auxDelEdge nID (tail xs)


{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
 -    This function taks as an input a NodeID and Graph a. It checks to see if the head Node in the inputted Graph a (where the node = fst(headxs)) has the
 same NodeId as the NodeID inputted. If there is such as Node, that Node is returned as a Maybe type. Otherwise, the function lookupNode is called again 
 with the same NodeId as input and the tail of the Graph. If an empty Graph is inputted, Nothing is returned.
 -}

lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph []) = Nothing 
lookupNode nID (Graph xs) 
  |getNodeID node == nID = Just node
  |otherwise = lookupNode nID (Graph (tail xs))
  where 
    node = fst (head xs)


{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
 -   This function takes in a tuple containing two NodeID and a Graph a (of any type). The function calls on the function containsBothNodes to check if 
 both NodeIds exist in the graph. This is done by calling on the function lookupNode with each of the NodeIDs in the tuple. If either of these calls return
 Nothing, insertEdge will return Nothing. Otherwise, the function calls on the auxilary function, auxInsert to insert the NodeId of the second element in 
 the tuple into the list of edges for the Node matching the NodeID of the first element in the tuple. insertEdge returns a new Graph of type Maybe with the
 Node matching the first NodeID in the tuple containing the second NodeID of the tuple as one of the elements of its edges.
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [])  = Nothing
insertEdge (n1,n2) g@(Graph graph)
  | not (containsBothNodes n1 n2) = Nothing
  | otherwise = Just (Graph (auxInsert n1 n2 (Graph graph)))
  where
    containsBothNodes :: NodeID -> NodeID -> Bool
    containsBothNodes n1 n2
      | lookupNode n1 (Graph graph) == Nothing = False
      | lookupNode n2 (Graph graph) == Nothing = False
      | otherwise = True

{- -----------------------------------------------------------------
 - auxInsert
 - -----------------------------------------------------------------
 - Description:
 -   This function takes in two NodeIds and a Graph a. If the Graph inputted is empty, an empty Graph is returned. Else, the function traverses through the
 Graph to find the Node that matches the first NodeID (by comparing the first NodeId to the NodeId of the head Node of the graph). When it finds the Node, 
 it concantenates the node with the function checkDup called on its list of edges, to the remaining list of Nodes and returns this result as type [(Node a,
 [NodeID])] (which is the same type as a Node).
 -}
auxInsert :: NodeID -> NodeID -> Graph a -> [(Node a, [NodeID])]
auxInsert n1 n2 (Graph []) = []
auxInsert n1 n2 (Graph xs) 
   |nodeID == n1 = (Node nodeID nodeVal, checkDup nodeList n2) : tail xs
   |otherwise = head xs : auxInsert n1 n2 (Graph (tail xs))
  where 
   nodeID = getNodeID (fst (head xs))
   nodeVal = getNodeVal (fst (head xs))
   nodeList = snd (head xs)

{- -----------------------------------------------------------------
 - checkDup
 - -----------------------------------------------------------------
 - Description:
 -   This function takes as an input a list of NodeID and a NodeID. If the list is empty, it returns a list with the sole NodeID (adds the edge to the 
 list). Otherwise it traverses the list to check if any of the elements are the same as the the given NodeID (so that no duplicates will be added.) This is
 done by checking head of list against the NodeID. If they match, the list unchanged is returned. If they do not match, the head of the list is 
 concatenated with another call of the function checkDup on the tail of the list with the original NodeID value. This way, if the edge already exists in 
 the list, a duplicate will not be added and the list will be returned unchanged. Otherwise, if the whole list is tranvered and the element is not found, 
 the edge is added to the end of the list and returned.
 -}
checkDup :: [NodeID] -> NodeID -> [NodeID]
checkDup [] n2 = [n2]
checkDup xs n2
   | head xs == n2 = xs
   | otherwise = head xs : checkDup (tail xs) n2
