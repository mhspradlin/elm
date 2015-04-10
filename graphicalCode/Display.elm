-- Display.elm
-- This code manages the top-level display and layout of the page.

-- Mitch Spradlin
-- 2015-04-05

import Signal
import Window
import String
import Mouse
import Graphics.Element as E
import Graphics.Collage as GC
import Html
import Html.Attributes
import Html.Events
import ToyLang
import List
import Maybe
import Debug

--The current model is the input string and if one of the rendered objects is 
-- being altered (mouse is over and clicked down), which includes tracking the
-- rendered objects and what variables in the input code determine their
-- characteristics
type alias Model = { code : String
                   , objects : List Object
                   , movingObj : Maybe (Object, Float, Float)
                   }

--The information we need for an object is something to deduce its x and y
-- and the character locations in the code that represents the x and y positions
type alias Object = (GC.Form, Int, Int)

--The types of events that can be sent to our program
--The Dragging type is (mousex, mousey) thingbeingdragged distancetomaintain
type Event = UpdateCode String
           | MouseDown (Int, Int)
           | MouseUp

--The channel over which events are sent to our application
eventChan : Signal.Channel Event
eventChan = Signal.channel <| UpdateCode ""

--Update the given state to reflect either a change in the code or a change in
-- the graphics, and changes the other to bring them into sync
--At the moment, 'change in the code' = 'code text has changed'
--               'change in the graphics' = 'object is being dragged'
upstate : Event -> Model -> Model
upstate event state = case event of
    UpdateCode newcode -> { state | code <- newcode
                                  , objects <- ToyLang.parseCode newcode 
                          }
    MouseUp -> { state | movingObj <- Nothing }
    MouseDown (mx, my) -> case state.movingObj of
        Nothing ->
            let maybeObj = pickObj (mx, my) state.objects
            in case maybeObj of
                Nothing -> state
                Just (form, x, y) -> { state | movingObj <- Just ((form, x, y)
                                                                 , form.x -
                                                                    toFloat mx
                                                                 , form.y -
                                                                    toFloat my
                                                                 )
                                     }
        Just (obj, xdist, ydist) ->
            let newpos = (toFloat mx + xdist, toFloat my + ydist)
                newobjs = List.map (updateObjPos newpos obj)
                                    state.objects
                newcode = ToyLang.parseObject (updateObjPos newpos obj obj) state.code
            in  { state | code <- newcode
                        , objects <- newobjs 
                        , movingObj <- Just 
                            (updateObjPos newpos obj obj, xdist, ydist)
                }

--Given a position and a list of objects, picks at most one where the position
-- is within the object
--Note that we defined the edge length of the square to be 40 in ToyLang
pickObj : (Int, Int) -> List Object -> Maybe Object
pickObj (mx, my) objs = case objs of
    [] -> Nothing
    (form, x, y) :: xs -> if | abs (form.x - toFloat mx) <= 20 
                               && abs (form.y - toFloat my) <= 20 -> Just (form, x, y)
                             | otherwise -> pickObj (mx, my) xs

--Updates the position of an object if it is being dragged
--Currently done with a rather crude check of equality on two Objects, which
-- may not uniquely determine an object
updateObjPos : (Float, Float) -> Object -> Object -> Object
updateObjPos (newx, newy) obj other = if
    | obj == other -> let (form, xpos, ypos) = obj
                      in ({ form | x <- newx, y <- newy }, xpos, ypos)
    | otherwise -> other

--If the transition is True -> False, then the mouse was lifted
mouseUp : Bool -> (Bool, Maybe Event) -> (Bool, Maybe Event)
mouseUp newevt (oldevt, _) = case (oldevt, newevt) of
    (True, False) -> (newevt, Just MouseUp)
    _             -> (newevt, Nothing)

--Performs the coordinate adjustment necessary for the mouse position to
-- correspond to the canvas position of the squares
adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - 3 * (w // 4), (-1 * my) + h // 2)

--The initial state
initState = { code = "", objects = [], movingObj = Nothing }

--The code box on the left
codeBox : String -> Html.Html
codeBox codeText =
    Html.textarea
        [ Html.Attributes.id "codeBox"
        , Html.Attributes.value codeText
        , Html.Events.on "input" Html.Events.targetValue 
            (Signal.send eventChan << UpdateCode)
        ]
        []

--Renders the input code on the left and what it represents on the right 
view : (Int, Int) -> Model -> E.Element
view (w,h) state = E.flow E.right [ Html.toElement (w // 2) h
                                        <| codeBox state.code
                                  , GC.collage (w // 2) h
                                        <| List.map (\(a,b,c) -> a) state.objects
                                  ]

--A canonical main
main : Signal E.Element
main = let sigState = Signal.foldp upstate initState 
                                   <| Signal.map (Debug.watch "eventSig")
                                   (Signal.mergeMany 
                                        [ Signal.subscribe eventChan
                                        , Signal.map (\(a,Just x) -> x)
                                            <| Signal.keepIf (\(a,x) -> x /= Nothing)
                                                             (False, Just
                                                                     MouseUp)
                                            <| Signal.foldp mouseUp 
                                                            (False, Nothing)
                                                            Mouse.isDown 
                                        , Signal.map MouseDown
                                            <| Signal.map2 adjustCoords
                                                           Window.dimensions
                                            <| (Signal.keepWhen Mouse.isDown
                                                                  (0,0)
                                                                  Mouse.position)
                                        ]
                                   )
       in Signal.map2 view Window.dimensions sigState
