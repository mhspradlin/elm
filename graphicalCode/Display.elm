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

--The types of actions that can occur in our application

--Update the given state to reflect either a change in the code or a change in
-- the graphics, and changes the other to bring them into sync
--At the moment, 'change in the code' = 'code text has changed'
--               'change in the graphics' = 'object is being dragged'
upstate : Bool -> (Int, Int) -> (Model, Model) -> (Model, Model)
upstate mousedown mousepos (state, oldstate) = 

--The initial state
initState = { code = "", objects = [] }

--Renders the input code on the left and what it represents on the right 
view : (Int, Int) -> Model -> Element
view (w,h) 

--A canonical main
main : Signal Element
main = let sigState = Signal.foldp (Signal.map2 Mouse.
Signal.map2 view Window.dimensions (Signal.foldp upstate initState)
