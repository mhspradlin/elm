-- ToyLang.elm
-- This code defines a toy language for rendering simple graphics.

-- Mitch Spradlin
-- 2015-04-05

module ToyLang where

--Result library
import Result as R
--Collage library
import Graphics.Collage as GC
import Color as C
--List library
import List ((::))
import List
--String library
import String

--The possible tokens that can be used in the toy language 
type Token = Square (Var Float) (Var Float) | And

--Contains a value and the position in the input code that it is represented at
type alias Var a = (a, Int)

--To parse the language, we first reduce it to tokens, perform any operations
-- that need to be to deduce the final positions of the elements (none at the
-- moment), and then transform them into elements
--Parses an input of 'square x y' where x and y need to be integers
--Returns either a valid token or an error
square : String -> Result String Token
square instr = case String.left 6 instr of
    "" -> Err "Empty"
    "square" -> case List.map String.toFloat <| List.take 2 <| List.drop 1 <| String.split " " instr of
        [] -> Err "Not enough arguments to square"
        [x] -> Err "Only one argument to square given"
        [Err x, _] -> Err x
        [_, Err y] -> Err y
        [Ok x, Ok y] -> let (xpos,ypos) = findArgPos instr
                        in Ok (Square (x, xpos) (y, ypos))
    _ -> Err "Invalid input"

--Takes in a well-formed square statement and a current position tuple, then
-- gives the position in the string that the x and y arguments are in, zero 
-- indexed
findArgPos : String -> (Int, Int)
findArgPos instr = case String.indexes " " instr of
    sp1 :: sp2 :: rest -> (sp1 + 1, sp2 + 1)
    _ -> (-1, -1) --Poorly formatted, not going to happen (hopefully)

--Parses an input of 'and'
--Returns either a valid token or an error
and : String -> Result String Token
and instr = case String.left 3 instr of
    "" -> Err "Empty"
    "and" -> Ok And
    _ -> Err "Invalid input"

--Shifts all indices in later tokens to account for offsets
shiftIndices : Int -> List Token -> List Token
shiftIndices shamt l = case l of
    [] -> []
    And :: ls -> And :: shiftIndices shamt ls
    Square (x, xpos) (y, ypos) :: ls -> 
        Square (x, xpos + shamt) (y, ypos + shamt) :: shiftIndices shamt ls
    _ -> [] --Uh oh

--Actually perform the tokenization
tokenize : String -> List Token
tokenize instr = case (square instr, and instr) of
    (Err x, Ok y) -> y :: (shiftIndices 3 <| tokenize <| String.dropLeft 3 instr)
    (Ok x, Err y) -> case String.indexes " " instr of
        [_, _] -> x :: []
        _ :: _ :: n :: ns -> 
            x :: (shiftIndices n <| tokenize <| String.dropLeft n instr)
    _ -> case String.left 1 instr of
        "" -> []
        " " -> shiftIndices 1 <| tokenize <| String.dropLeft 1 instr
        _ -> [] --Parse error

--Returns if something is an And or not
isAnd : Token -> Bool
isAnd token = case token of
    And -> True
    _ -> False

--Converts a token into a (Form, Int, Int) tuple if appropriate
toForm : Token -> (GC.Form, Int, Int)
toForm token = case token of
    Square (x, xpos) (y, ypos) -> 
        let form = GC.move (x, y) <| GC.filled C.lightBlue <| GC.square 40 
        in (form, xpos, ypos)

--The result of parsing this language will be a list of positioned forms with two
-- positions in the code where the xpos and ypos of the square is represented
parseCode : String -> List (GC.Form, Int, Int)
parseCode code = List.filter (not << isAnd) (tokenize code) |> List.map toForm

--Going the other way, we can take one input Object, a reference set of code,
-- and then make sure that the associated constants are changed to represent its
-- position
--There is a long way to go with this one; probably need to ID the objects and
-- associated constants and then related parts of syntax... hmm...
parseObjects : (GC.Form, Int, Int) -> String -> String
parseObjects (obj, xpos, ypos) oldcode =
    let xstr = toString obj.x
        ystr = toString obj.y
    in subStr xstr xpos oldcode |> subStr ystr ypos

--Takes a string to substitute, a position of that string, and makes the
-- substitution. Does absolutely no checking of if anything is at all how it's
-- supposed to be.
subStr : String -> Int -> String -> String
subStr newVal valpos oldcode = String.concat
    [ String.left valpos oldcode
    , newVal
    , String.dropLeft valpos oldcode |> String.split " " |> List.drop 1 |>
      (\x -> if x == [] then "" else " " ++ String.join " " x)
    ]
