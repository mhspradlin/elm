-- Chat.elm
-- This is the main elm file for a decentralized chat client.

-- Mitch Spradlin
-- 2015-02-21

module Chat where

import Graphics.Element as E
import Debug
import Signal
import String
import Text as T
import Graphics.Input as I
import Graphics.Input.Field as F
import List ((::))
import List
import Window
import Keyboard
import Html --So we can style our things much better
import Html.Lazy
import Html.Attributes (..)
import Html.Events
import Json.Decode as Json
import Time
import Maybe
import Graphics.Collage as C
import Color
import Random
import World (..)

--The Events that can happen to alter the state
type Event = Enter Bool
            | Cmd String
            | BoxUpdate F.Content

--A list of possible command strings that we can send to JS
--Used during development, probably will eliminate before demo time
commands : List String
commands = 
    [ "makePeer"
    , "myID"
    , "connect"
    , "send"
    , "listConns"
    , "destroySelf"
    , "setName"
    , "connectToExisting"
    , "getExisting"
    ]

--A button channel so that we can do things
buttonChannel : Signal.Channel String
buttonChannel = Signal.channel ""

--A Content channel so that we can interact with a text box
boxChannel : Signal.Channel F.Content
boxChannel = Signal.channel F.noContent

--A nice alias
noContent : F.Content
noContent = F.noContent

--This is where we get information from the peer.js code 
port fromJS : Signal String

--This is where we send information to the js
fromElmChannel : Signal.Channel String
fromElmChannel = Signal.channel ""
port fromElm : Signal String
port fromElm = Signal.mergeMany 
                [ Signal.subscribe fromElmChannel
                , Signal.dropIf (String.startsWith "noOp") "noOp"
                  <| Signal.dropRepeats --BEWARE, updates occasionally get
                                     -- clobbered by forwards
                  <| Signal.map forward sigState
                , Signal.dropIf (String.startsWith "noOp") "noOp"
                  <| Signal.map timeUpdate
                  <| Signal.sampleOn (Time.every Time.second)
                  <| Signal.map2 (,) (Time.every Time.millisecond) sigState                        
                ]
                    
--Because this is so annoying
--This feeling has been replaced by shame
fromJust : Maybe a -> a
fromJust (Just x) = x

--The signal of our states
sigState : Signal State
sigState = let eventSig = Signal.mergeMany 
                        [ Signal.map Cmd fromJS
                        , Signal.map BoxUpdate (Signal.subscribe boxChannel)
                        , Signal.foldp (\x (y,z) -> (x,y)) 
                                (False,False)
                                Keyboard.enter
                            |> Signal.map (\(x,y) -> (x,y) == (True,False))
                            |> Signal.map Enter
                        ]
       in
        Signal.foldp upstate initState eventSig

--Takes a string and splits it once at the given delimiter
splitOnce : String -> String -> String -> List String
splitOnce delim str accum = case String.uncons str of
    Nothing -> []
    Just (c, rest) -> if | String.fromChar c == delim -> [accum, rest]
                         | otherwise -> splitOnce delim rest
                                        (String.append rest (String.fromChar c))


--Takes events and updates the state appropriately (think foldp)
upstate : Event -> State -> State
upstate event state = case event of
    Enter True -> { state | messages <- String.append "> " state.textBox.string :: state.messages
                     , textBox <- F.noContent } 
    Enter False -> state --Annoying
    Cmd msg -> parseMsg msg state
    BoxUpdate newContent -> { state | textBox <- newContent }

--Takes a time and a state and generates a string that is sent to the JavaScript
-- that represents some sort of time-dependent update that is made
--This CANNOT influence the state - if I want to do something like timeout a
-- stale connection this needs to send a command to the JavaScript that, upon
-- returning, will update the state
--This function will do more than I originally intended
--This is really the place to do things like, if we're not connected try to
-- connect to the server and get an ID, and if we're connected, send a
-- 'heartbeat' update to the others
--The updates we send to others only need to be of the edges, as there will be
-- no singleton nodes in the network
timeUpdate : (Time.Time, State) -> String
timeUpdate (time, state) = case state.self of
    Nothing -> case state.connecting of
        True -> "noOp"
        False -> let tryID = round (Time.inSeconds time) % 100
                 in
                    String.append "makePeer:" (toString tryID)
    Just self  -> case state.connections of
        [] -> if --Make the initial connections slowly
                 | round (Time.inSeconds time) % 3 == 0 -> "connectToExisting"
                 | otherwise -> "noOp"
        _ -> "getExisting" --syncExisting and update once a second
             

--Takes an edge and makes a JSON approprate string
makeString : (Connection, Connection) -> String
makeString (c1, c2) = String.concat
    [ "[{"
    , "\"id\":\""
    , c1.id
    , "\",\"name\":\""
    , c1.name
    , "\"},{\"id\":\""
    , c2.id
    , "\",\"name\":\""
    , c2.name
    , "\"}]"
    ]

--Takes a State, looks at the queue, and forwards the head of it
--Should get all of the additions to the queue, as it will be called every time
-- that the queue is updated - synchronicity and all that
--Figures out if there is anyone we're connected to that hasn't been sent the
-- message yet, sends it to them and appends their ids to the trace
forward : State -> String
forward state = case state.queue of
    [] -> "noOp"
    x :: _ -> case String.split ":" x of
        "send" :: "all" :: _ -> x
        "connect" :: id :: [] -> x
        trace :: msg :: _ -> 
            let past = String.split "," trace
                future = List.filter (\x -> List.all ((/=) x.id) past) 
                                  state.connections
                futureList = String.join "," (List.map (\x -> x.id) future)
                futureTrace = String.concat [futureList, ",", trace]
            in
               case future of
                   [] -> "fwd"
                   _ -> String.join ":" ["fwd"
                                        , "send"
                                        , futureList
                                        , futureTrace
                                        , msg
                                        ]

--This takes an string message formatted in the fromJS style and updates a given
-- State appropriately (think Signal.foldp)
parseMsg : String -> State -> State
parseMsg msg instate = 
    let clearFluff = if msg == "clearQueue" then [] else [msg]
        state = { instate | sysMessages <- List.append clearFluff 
                              <| List.take 10 instate.sysMessages }
    in case String.split ":" msg of
        []           -> state 
        "attemptingServerConn" :: xs -> { state | connecting <- True }
        "lostConn" :: xs -> { state | connecting <- False
                                    , self <- Nothing
                                    , connections <- [] }
        "myID" :: xs -> { state | self <- Just { id = List.head xs
                                          , name = "unassigned"
                                          }
                                , picture <- { nodes = [
                                                { id = List.head xs
                                                , name = "unassigned"
                                                }
                                                ]
                                              , edges = []
                                              }
                                , connecting <- False
                                , messages <-
                                    ("Once connected to a peer, simply type a"
                                    ++
                                    " message and hit enter to send it!")
                                     :: ("Set your nickname with the setName"
                                          ++
                                        " command, as indicated on the right.")
                                     :: ("You have retrieved an ID"
                                          ++
                                        " and are searching for peers.")
                                     :: "Welcome to peerChat!"
                                     :: state.messages }
        "myName" :: xs -> let self = fromJust state.self in --guaranteed that peer
                                                            -- exists
            { state | self <- Just { self | name <- List.head xs }
                    , picture <- updateName { self | name <- List.head xs }
                                            state.picture    
            }
        "clearQueue" :: xs -> { state | queue <- List.drop 1 state.queue } --Should rearchitect to avoid
                                                      -- such things...
        "disconnectedFrom" :: xs -> parseDisconnect xs state
        "connectedTo" :: xs -> let newConn = { id = List.head xs
                                             , name = "unassigned"
                                             }
                                   self = fromJust state.self
                                   newstate =
                                       { state | connections <- newConn ::
                                                    state.connections
                                               , picture <- addEdge (self, newConn)
                                                                state.picture
                                       }
                               in newstate 
        "existing" :: xs -> syncExisting xs state
        "data" :: xs -> parseData xs state
        "err" :: xs -> state --Potential future use
        _           -> state --A system message in this case 

--Takes a data input string and returns an appropriately modified state
--Most of these will be routine pings from the connected peers which might
-- modify the current 'picture' of the world if their information is more recent
-- than our own
parseData : List String -> State -> State
parseData xs state = case xs of
    [] -> state --Is ill-formatted, TODO should report error
    [x] -> state --Is ill-formatted, TODO should report error
    peerID :: content :: [] -> if --A message that's meant to end with us 
        | String.startsWith "msg" content -> 
            { state | messages <- (String.concat
                                    [ "("
                                    , peerID
                                    , ") "
                                    , (\x -> x.name) <| List.head <| List.filter
                                      (\x -> x.id == peerID) state.picture.nodes
                                    , ": "
                                    , String.dropLeft 3 content
                                    ]) :: state.messages
            }
        | String.startsWith "upd" content ->
            mergeUpdate (String.join "," <| List.drop 2 <| String.split "," content) state
        | otherwise -> state --gobbledegook
    peerID :: trace :: content :: rest -> if -- A message that's meant to go on
        | String.startsWith "msg" content -> --The last item of trace is from
           (let fromid = List.head <| List.reverse <| String.split "," trace
                probablyduplicate = (Debug.log "content" [content]) == (Debug.log "lastcontent" (List.drop 2 
                                                   <| String.split ":"
                                                             state.lastMsg))
            in if
                | probablyduplicate -> state
                | otherwise ->
                    { state | lastMsg <- String.join ":"  
                                            <| List.append 
                                            [ peerID
                                            , fromid 
                                            , content
                                            ]
                                            rest
                            , messages <- (String.concat
                                            [ "("
                                            , List.head <| List.reverse 
                                              <| String.split "," trace
                                            , ") "
                                            , (\x -> x.name) <| List.head <| List.filter
                                                (\x -> x.id ==  
                                                (List.head <| List.reverse 
                                                <| String.split "," trace))
                                              state.picture.nodes
                                            , ": "
                                            , String.join ":"
                                                ((String.join "," <| List.drop 2 
                                                  <| String.split "," content)
                                                                   :: rest)
                                            ]) :: state.messages
                            , queue <- (String.join ":" [trace, content]) :: 
                                            List.take 14 state.queue
                    })
        | String.startsWith "upd" trace -> --updates have no trace
            mergeUpdate (String.join ":" (String.dropLeft 3 trace :: content :: rest))
                        state
        | String.startsWith "dis" content -> --Another node disconnected
           (let disconn = String.dropLeft 3 content
                probablyduplicate = [String.concat 
                                        [ "User "
                                        , disconn
                                        , " disconnected"
                                        ]] == List.take 1 state.messages
            in if
                | probablyduplicate -> state
                | otherwise ->
                    { state | picture <- remNode disconn state.picture
                            , messages <- String.concat
                                            [ "User "
                                            , disconn
                                            , " disconnected"
                                            ] :: state.messages
                            , queue <- (String.join ":" [trace, content]) ::
                                            List.take 14 state.queue
                    })
        | otherwise -> state --gobbledegook

--Takes an input string (from the text box) and decides what to actually send to
-- the JS
--Takes in a list of connections to add a trace to the message being sent
--Make sure the trace includes own ID at the end - how we know where it came
-- from
parseInput : String -> State -> Int -> String
parseInput instr state salt = if
    | List.any (\x -> String.startsWith x instr) commands -> instr
    | otherwise -> let self = case state.self of
                                Nothing -> { id = "", name = "" }
                                Just x -> x
                       trace = String.join "," <| List.map (\x -> x.id)
                               (List.append state.connections [self])
                       recipients = String.join "," <| List.map (\x -> x.id)
                                state.connections
                       msgID = fst <| Random.generate (Random.int 1 999999)
                                               (Random.initialSeed salt)
                       content = String.join "," [ "msg"
                                                 , toString msgID
                                                 , instr
                                                 ]
                   in String.join ":" ["send", recipients, trace, content]

--Updates a state to reflect a disconnect from a connected user, updating the
-- world view appropriately 
--Also sends out a message that this user is no longer part of the network
parseDisconnect : List String -> State -> State
parseDisconnect [did] state = 
    let self = case state.self of
                Nothing -> { id = "", name = "" }
                Just x -> x
        trace = self.id --forward will handle adding the others
        newconnections = List.filter (\x -> x.id /= did)
                                     state.connections
    in 
       { state | connections <- newconnections
               , picture <- case newconnections of
                   [] -> { nodes = [], edges = [] }
                   _  -> remNode did state.picture
               , messages <- String.concat
                                [ "User "
                                , did
                                , " disconnected"
                                ] :: state.messages
               , queue <- String.join ":" [ trace
                                          , String.append "dis" did
                                          ]  :: List.take 14 state.queue
       }

--Takes in a list whose head is a string that represents the IDs of all nodes
-- that exist at that moment
--This should remove all those in the current picture who are not in this list
--As people only send out updates when their status changes, the adjacent people
-- will only send out information that is newer - as in, no phantom nodes should
-- be added by neighbors after this one has been updated
--It's possible that there will be some jitter when a new person is added, gets
-- bad info, then sends out an update to potentially one other who will
-- propagate the bad info - it should be resolved quickly though so long as not
-- many new people keep getting added
syncExisting : List String -> State -> State
syncExisting xs state = case xs of
    [] -> state --Not formed well
    [x] -> let existingNodes = case Json.decodeString --What it should be
                    (Json.list Json.string) x of
                        Ok z -> z
                        Err _ -> []
               extraNodes = List.filter 
                                (\x -> not <| List.member x.id existingNodes)
                                state.picture.nodes
               extraNodeIDs = List.map (\x -> x.id) extraNodes
               newPicture = case extraNodeIDs of
                   [] -> state.picture
                   _  -> List.foldl (\x world -> remNode x world) state.picture
                                                                  extraNodeIDs
               sendUpdate = 
                    String.concat
                        [ "send:all:upd"
                        , "["
                        , String.join ","
                            (List.map makeString newPicture.edges)
                        , "]"
                        ]
           in { state | picture <- newPicture 
                      , queue <- sendUpdate :: state.queue }
    _ -> state --Also not formed well
                    

--Takes an update from another peer, binPacked, and updates our picture if need
-- be
mergeUpdate : String -> State -> State
mergeUpdate update state = 
    let theirPictureResult = Json.decodeString 
            (Json.list 
                (Json.tuple2 (,) 
                    (Json.object2 Connection 
                        (Json.at ["id"] Json.string)
                        (Json.at ["name"] Json.string)
                    )
                    (Json.object2 Connection
                        (Json.at ["id"] Json.string)
                        (Json.at ["name"] Json.string)
                    )
                )
            )
            <| update
        theirPicture = case theirPictureResult of
            Err x -> []
            Ok x -> x
        mergeedge : (Connection, Connection) -> World -> World
        mergeedge = \(x,y) z -> updateName x <| updateName y <| addEdge (x,y) z
        newstate = { state | picture <- List.foldr mergeedge state.picture theirPicture }
    in 
        case nextConnection newstate of
            Nothing -> newstate
            Just x -> { newstate | queue <- String.append "connect:" x.id
                                                   :: state.queue
                      }
        
                
--Takes desired element size, a world, and produces an element that is a
-- visualization of the world
visualizeWorld : (Int, Int) -> State -> E.Element
visualizeWorld (w,h) state =
    let world = state.picture
        myMin = (\x y -> if x < y then x else y)
        radius = (myMin w h) // 2 - 20
        defaultStyle = T.defaultStyle
        idStyle = { defaultStyle | height <- Just 14
                                 , bold <- True
                                 , color <- Color.white }
        circleColor = (\x -> if | state.self == Nothing -> Color.lightBlue
                                | otherwise -> 
                                    if | x.id == ((\y -> y.id) 
                                                  <| fromJust state.self) ->
                                            Color.lightRed
                                       | otherwise -> Color.lightBlue)
        circle = (\x -> C.group 
                            [ C.filled (circleColor x) <| C.circle 20
                            , C.toForm (T.leftAligned <| 
                                        T.style idStyle <| T.fromString x.id)]
                 )
        indexedNodes = List.indexedMap (,) world.nodes
        steps = List.length world.nodes
        thetaN = (\x -> (2 * pi * (toFloat x)) / (toFloat steps))
        xTheta = (\i -> toFloat radius * cos (thetaN i))
        yTheta = (\i -> toFloat radius * sin (thetaN i))
        placeNode = (\(i,x) -> C.move (xTheta i, yTheta i)
                                 <| circle x)
        nodes = List.map placeNode indexedNodes
        getNodePos = (\x -> (\(i,z) -> (xTheta i, yTheta i))
                            <| List.head 
                            <| List.filter (\(b,y) -> y.id == x.id) indexedNodes)
        makeEdge = (\(x,y) -> C.traced (C.solid Color.black) 
                                <| C.segment (getNodePos x) (getNodePos y))
        edges = List.map makeEdge world.edges
        listforms = List.append edges nodes
   in C.collage w h listforms
       
--Set up a few buttons for testing the signals and printing the output we get
-- from the 
--Takes the current state, screen dimensions, and the content of the text box
view : State -> (Int, Int) -> E.Element
view state (w,h) = 
    let defaultStyle = F.defaultStyle
        outline = defaultStyle.outline
        textBox = Html.toElement (w // 2 - 10) 50 
                     <| Html.Lazy.lazy 
                        (\x -> Html.input 
                            [ id "inputField"
                            , placeholder "Input messages here"
                            , autofocus True
                            , value x
                            , name "inputField"
                            , Html.Events.on "input" 
                                Html.Events.targetValue 
                                (Signal.send boxChannel 
                                    << (\x ->  { noContent | string <- x }))
                            , Html.Events.on "keydown"
                                (Json.customDecoder Html.Events.keyCode
                                    (\x -> if x == 13 then Ok () else Err
                                              "Nah"))
                                <| always <| Signal.send fromElmChannel
                                                (parseInput state.textBox.string
                                                            state
                                                            (w * h * String.length
                                                             state.textBox.string))
                            ]
                            [])
                        (state.textBox.string)
        textDefaultStyle = T.defaultStyle
        fontfaces = ["Verdana", "Geneva", "sans-serif"]
        messageStyle = { textDefaultStyle | typeface <- fontfaces }
        headerStyle = { messageStyle | height <- Just 18, bold <- True } 
        titleStyle = { messageStyle | bold <- True }
        styleMap style = List.map (T.fromString >> T.style style >>
                                T.leftAligned)
        messageBox = E.container (w // 2 - 10) (h - 90) E.bottomLeft
                     <| E.flow E.up
                     <| styleMap messageStyle
                     <| List.take 40 state.messages
        boxesOfText = E.flow E.down [messageBox, E.spacer (w // 2 - 10) 10, textBox]
        helpText = E.flow E.down
                    <| List.append (styleMap titleStyle ["Available commands:"])
                        (styleMap messageStyle 
                        [ "    setName:Name -- Sets own name (may only be done once)"
                        --, "makePeer:ID -- requests the given ID from the server"
                        --, "myID -- returns your unique chat ID"
                        , "    connect:ID -- attempts to connect to an ID"
                        --, "send:ID:Message -- attempts to send Message to ID"
                        --, "listConns -- lists IDs currently connected to"
                        ])
        selfText = case state.self of
            Just self -> String.concat [ "    ID: "
                                       , self.id
                                       , String.fromChar '\n'
                                       , "    Name: "
                                       , self.name]
            Nothing -> "Have not obtained ID yet!"
        connectionText = List.map toString state.connections 
        statusText = E.flow E.down
                                <| List.concat
                                [ styleMap titleStyle ["You are:"]
                                , styleMap messageStyle [selfText]
                                --, ["Connected to:"]
                                --, connectionText
                                ]
        sysMessageText = styleMap messageStyle 
                            <| List.map (String.append "    ") state.sysMessages
        systemText = E.flow E.down
                                <| List.concat
                                [ styleMap titleStyle ["System Messages:"]
                                , sysMessageText
                                ]
    in 
        E.flow E.right [ boxesOfText
                       , E.spacer 20 (h - 20)
                       , E.flow E.down 
                            [ T.fromString "Known Connections"
                                |> T.style headerStyle
                                |> T.centered
                                |> E.container (w // 2 - 10) 30 E.middle
                            , visualizeWorld (w // 2 - 10, h // 2) state
                            , statusText
                            , helpText
                            , E.container ( w // 2 - 10) (h // 4) E.topLeft
                                <| systemText
                            ]
                       ] 

--The entry point for our program
main : Signal E.Element
main = Signal.map2 view sigState Window.dimensions
