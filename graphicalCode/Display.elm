-- Display.elm
-- This code manages the top-level display and layout of the page.

-- Mitch Spradlin
-- 2015-04-05

import Signal
import Window
import String
import Mouse
import Graphics.Collage as GC
import Html
import Html.Attributes
import Html.Events
import ToyLang

--The current model is the input string and if one of the rendered objects is 
-- being altered (mouse is over and clicked down), which includes tracking the
-- rendered objects and what variables in the input code determine their
-- characteristics
type alias Model = { code : String
                   , objects : List Object
                   }

--The information we need for an object is something to deduce its x and y
-- and the character locations in the code that represents the x and y positions
type alias Object = (GC.Form, Int, Int)

--The types of events that can be sent to our program
--The Dragging type is (mousex, mousey) thingbeingdragged distancetomaintain
type Event = UpdateCode String
           | Dragging (Int, Int) Object (Float, Float)

--The channel over which events are sent to our application
eventChan : Signal.Channel
eventChan = Signal.channel <| UpdateCode ""

--Update the given state to reflect either a change in the code or a change in
-- the graphics, and changes the other to bring them into sync
--At the moment, 'change in the code' = 'code text has changed'
--               'change in the graphics' = 'object is being dragged'
upstate : Event -> Model -> Model
upstate event state = case update of
    UpdateCode newcode -> { state | code <- newcode
                                  , objects <- ToyLang.parse newcode 
                          }
    Dragging (mx, my) obj (xdist, ydist) -> 
        let newobjs = List.map (updateObjPos (mx,my) obj (xdist,ydist))
                                state.objects
        in 

--Updates the position of an object if it is being dragged
--Currently done with a rather crude check of equality on two Objects, which
-- does not uniquely determine an object
updateObjPos : (Int, Int) -> Object -> (Float, Float) -> 

--Given a list of Objects, update the code to reflect their actual positions


--The initial state
initState = { code = "", objects = [] }

--The code box on the left
codeBox : String -> Html
codeBox codeText =
    Html.textArea
        [ Html.Attributes.id "codeBox"
        , Html.Attributes.value codeText
        , Html.Attributes.on "input" Html.Events.targetValue 

--Renders the input code on the left and what it represents on the right 
view : (Int, Int) -> Model -> Element
view (w,h) 

--A canonical main
main : Signal Element
main = let sigState = Signal.foldp (Signal.map2 Mouse.
Signal.map2 view Window.dimensions (Signal.foldp upstate initState)
