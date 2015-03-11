-- World.elm
-- Exposes the World datatype and functions that operate on it for use in the
-- Chat.elm file. Also exposes the State data type, which represents the state
-- of the client at any given time.

-- Mitch Spradlin
-- 2015-03-07

module World where

import List ((::))
import List
import Maybe
import Graphics.Input.Field as F
import String

--A Connection object, which in Elm will be essentially a unique ID for the
-- connection that, when passed to the JavaScript, will map to a connection
-- 'object' that will have the appropriate method called on it
--In other words, we send a string to the JavaScript along the lines of
-- ID:Command:Content that will map to object.method(args) appropriately
type alias Connection = { id   : String
                        , name : String
                        }

--Represents known people as nodes and connections between them as edges
type alias World = { nodes : List Connection
                   , edges : List (Connection, Connection)
                   }


--The State that we need to keep track of at any given time
type alias State = { connections : List Connection
                   , lastMsg     : String
                   , messages    : List String
                   , textBox     : F.Content
                   , self        : Maybe Connection
                   , connecting  : Bool
                   , picture     : World
                   , queue       : List String
                   , sysMessages : List String
                   }

--The initial unpopulated state
initState : State
initState = { connections = []
            , lastMsg     = ""
            , messages    = []
            , textBox     = F.noContent
            , self        = Nothing
            , connecting  = False
            , picture     = { nodes = []
                            , edges = []
                            }
            , queue       = []
            , sysMessages = []
            }

--I feel shame
fromJust : Maybe a -> a
fromJust (Just x) = x

--Functions for helping us work with the World picture
--Adds an edge to the world, adding one or both parties to the world if
-- need be
--Do this only in terms of the IDs for now
addEdge : (Connection, Connection) -> World -> World
addEdge (c1, c2) world = 
    let worldEdgeIDs = List.map (\(x,y) -> (x.id,y.id)) world.edges
        worldNodeIDs = List.map (\x -> x.id) world.nodes
    in if
        | List.member c1.id worldNodeIDs && List.member c2.id worldNodeIDs -> (if
            | List.member (c1.id, c2.id) worldEdgeIDs || 
                List.member (c2.id, c1.id) worldEdgeIDs -> world
            | otherwise -> { world | edges <- (c1, c2) :: world.edges})
        | List.member c1.id worldNodeIDs -> 
            { world | nodes <- c2 :: world.nodes
                    , edges <- (c1, c2) :: world.edges }
        | List.member c2.id worldNodeIDs ->
            { world | nodes <- c1 :: world.nodes
                    , edges <- (c1, c2) :: world.edges }
        | otherwise -> { world | nodes <- c1 :: c2 :: world.nodes
                               , edges <- (c1, c2) :: world.edges }

--This function takes a connection and a world, and uses the first to set the
-- names of all the connections in the list of nodes and edges that share an ID
--Already done
setName : Connection -> World -> World
setName conn world = let upConn new old = if old.id == new.id then new else old
                         upEdge new (old1, old2) = (upConn new old1, 
                                                    upConn new old2)
                         newNodes = List.map (upConn conn) world.nodes
                         newEdges = List.map (upEdge conn) world.edges
                     in { world | nodes <- newNodes
                                , edges <- newEdges }

--Removes a node from the world, taking all associated edges with it
--Only requires an ID
--TODO should also remove unreachable nodes
remNode : String -> World -> World
remNode c world = let newNodes = List.filter (\x -> x.id /= c) world.nodes
                      newEdges = List.filter (\(x,y) -> x.id /= c && y.id /= c)
                                             world.edges
                  in
                     { nodes = newNodes, edges = newEdges }

--Updates the name of a connection in the world
updateName : Connection -> World -> World
updateName conn world = if
    | conn.name == "unassigned" -> world
    | otherwise ->
        let updatenode = (\x y -> if | y.id == x.id -> { y | name <- x.name }
                                     | otherwise -> y )
        in
            { world | nodes <- List.map (updatenode conn) world.nodes
                    , edges <- List.map (\(x,y) -> 
                        (updatenode conn x, updatenode conn y)) world.edges
            }

--Determines if we need to make a new connection in the network
--The algorithm is as such (for now):
-- Do I have at least two connections?
--   No -> Of the people who have the least number of connections, do I have the
--         lowest ID?
--      No -> Do nothing
--      Yes -> Connect to farthest away person with least number of connections
--              and lowest ID
--   Yes -> Do I have the least number of connections?
--      No -> Do nothing
--      Yes -> Do I have the lowest ID of the group?
--          No -> Do nothing
--          Yes -> Do I have less than ln the number of nodes?
--              No -> Do nothing
--              Yes -> Connect to farthest away person with least number of
--                     connections
--This should make it so that only one person with any given state is trying to
-- make a connection, which might help with network stability
--Will likely need a function that returns the shortest path
--Ooh, what if I start at one node, take all nodes connected to, and use an
-- intermediate type like (curDest, pathLen, [stepsTaken (trace)]) to determine
-- if we should add any of the connected nodes to the list of paths
--Then, we filter this output list to get the shortest ones
--Getting the number of connections is easy - just count the number of times the
-- id occurs in the edges
generateDistances : Connection -> List (Connection, Connection) -> List (Connection, Int)
generateDistances self edges = 
    let allpaths = propagatePaths [(self, [self])] edges
        pathswithlengths = List.map (\(x,l) -> (x, List.length l)) allpaths
    in dropDuplicates pathswithlengths 

--A version of a deduplicate function that chooses the one with a shorter path
--Deduplicates a List (Connection, Int), choosing the one with the shorter path 
dropDuplicates : List (Connection, Int) -> List (Connection, Int)
dropDuplicates list = case list of
    [] -> []
    [x] -> [x]
    (x,i) :: (y,j) :: xs -> if 
        | x.id == y.id -> (if | i < j -> (x,i) :: dropDuplicates xs
                              | otherwise -> (y,j) :: dropDuplicates xs)
        | otherwise -> (x,i) :: (y,j) :: dropDuplicates xs

--Propagates paths given a list of paths
propagatePaths : List (Connection, List Connection) -> 
                    List (Connection, Connection) ->
                        List (Connection, List Connection)
propagatePaths paths edges =
    let steppedPaths = List.map (\x -> propagatePath x edges) paths
        accumPaths = List.foldl 
                        (\(l,x) (new, old) -> (List.append l new, x :: old))
                        ([], [])
                        steppedPaths
    in if | fst accumPaths == [] -> snd accumPaths
          | otherwise -> List.append (propagatePaths (fst accumPaths) edges)
                                     (snd accumPaths)

--Propagates one path, returning a tuple with 'new' paths on the left and the 
-- 'old' paths (given in the input) on the right
propagatePath : (Connection, List Connection) ->
                    List (Connection, Connection) ->
                        (List (Connection, List Connection), 
                            (Connection, List Connection))
                        
propagatePath (curDest, trace) edges =
    let adjacents = getConnections curDest edges
        notVisited = List.filter (\x -> not (List.member x trace)) adjacents
        newPaths = List.map (\x -> (x, x :: trace)) notVisited
    in (newPaths, (curDest, trace))

--Provides a list of Connections that are connected to the given Connection
getConnections : Connection -> List (Connection, Connection) -> List Connection
getConnections conn edges = case edges of
    [] -> []
    (x,y) :: xs -> if | conn.id == x.id -> y :: getConnections conn xs
                      | conn.id == y.id -> x :: getConnections conn xs
                      | otherwise -> getConnections conn xs

--Returns the id of a connection as an int
--Note that we can assume that these are well formed
connIntID : Connection -> Int
connIntID conn = case String.toInt conn.id of
    Ok x -> x
    Err _ -> -1

--Chooses the lower between two (conn, len) tuples
pickLower : (Connection, Int) -> (Connection, Int) -> (Connection, Int)
pickLower (x, l1) (y, l2) =
    if | connIntID x < connIntID y -> (x, l1)
       | otherwise -> (y, l2)

--Handles the situation of when we only have one connection
--Also works as a general 'find farthest + minID to connect to'
getConnection : State -> Maybe Connection
getConnection state = 
    let numconns = List.length state.connections
        numpeople = List.length state.picture.nodes
        allconns = List.map (\x -> (x, List.length <| getConnections x state.picture.edges))
                            state.picture.nodes
        loners = List.filter (\(x,l) -> l == 1) allconns
        minID = List.foldl pickLower (fromJust state.self, 0) 
                                     loners
    in 
       if | fst minID /= fromJust state.self -> Nothing
          | otherwise ->
            let alldistances = generateDistances (fromJust state.self)
                                                 state.picture.edges
                farthestdist = List.maximum <| List.map snd alldistances
                farthests = List.filter 
                             (\(x,l) -> l >= farthestdist)
                             alldistances
                smallID = List.foldl 
                            pickLower
                            ({ id = "101", name = "" }, 0)
                            farthests
            in Just <| fst smallID


--Handles the situation when we have many connections
manyConnections : State -> Maybe Connection
manyConnections state = 
   let numconns = List.length state.connections
       allconns = List.map
                    (\x -> List.length <| getConnections x state.picture.edges)
                    state.picture.nodes
       minall = List.minimum allconns
   in 
      if | numconns > minall -> Nothing
         | otherwise -> checkScale state <| getConnection state 

--Handles after we know we have one connection and have the minimum number of
-- connections
checkScale : State -> Maybe Connection -> Maybe Connection
checkScale state conn = case conn of
    Nothing -> Nothing
    Just x -> 
        if | List.length state.connections 
                >= round (logBase 2 <| toFloat 
                            <| List.length state.picture.nodes) ->
                    Nothing
           | otherwise -> 
              let alldistances = generateDistances 
                                  (fromJust state.self)
                                  state.picture.edges
                  farthestdist = List.maximum <| List.map snd alldistances
                  farthests = List.filter 
                               (\(x,l) -> l >= farthestdist)
                               alldistances
                  smallID = List.foldl 
                              pickLower
                              ({ id = "101", name = "" }, 0)
                              farthests
              in Just <| fst smallID

--Determines if we need to make a new connection in the network
nextConnection : State -> Maybe Connection
nextConnection state =
    let numconns = List.length state.connections
        numnodes = List.length state.picture.nodes
    in
        if | numnodes <= 2 -> Nothing
           | numconns <= 1 && numnodes > 2 -> getConnection state
           | otherwise -> manyConnections state
