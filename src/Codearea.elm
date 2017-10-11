port module Codearea exposing (..)

import Html exposing (Html, textarea, div, text)
import Html.Attributes exposing (id, class, property, value, cols, rows)
import Html.Events exposing (on, onWithOptions, onInput, defaultOptions, keyCode)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set, member)
import List exposing (foldl)

port setCursor : ( Int, String ) -> Cmd msg
port setSelection : ( Int, Int, String ) -> Cmd msg

type alias Model =
    { content : String
    , selectionStart : Int
    , selectionEnd : Int
    }

type Msg
    = SpecialKeyPress KeyboardEvent
    | KeyUp KeyboardEvent
    | Input String

main : Program Never Model Msg
main =
    Html.program
        { init = Model "" 0 0 ! []
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }

newlineExists : Int -> Int -> String -> Bool
newlineExists start end =
    String.contains "\n" << String.slice start end

stringWithoutChar : String -> Int -> String
stringWithoutChar string position =
    String.left (position - 1) string ++ (String.dropLeft position string)

stringWithoutSelection : String -> Int -> Int -> String
stringWithoutSelection string start end =
    String.left start string ++ (String.dropLeft end string)

modelWithChar : Model -> Char -> Int -> Int -> Model
modelWithChar model charToInsert selectionStart selectionEnd =
    { model
        | content = String.left selectionStart model.content ++ (String.cons charToInsert (String.dropLeft selectionEnd model.content))
        , selectionStart = selectionStart + 1
        , selectionEnd = selectionStart + 1
    }

withinRange : Int -> Int -> Int -> Bool
withinRange x start end =
    start <= x && x <= end

selectionIncludesLine : Int -> Int -> Int -> Int -> Bool
selectionIncludesLine lineIndex lineLength start end =
    withinRange start lineIndex (lineIndex + lineLength)
        || withinRange lineIndex start end

lineIndentedIfSelected : Int -> Int -> String -> ( String, Int, Int ) -> ( String, Int, Int )
lineIndentedIfSelected start end line ( content, index, indents ) =
    let
        length = String.length line
        newline = if index == 0 then "" else "\n"
    in
        if selectionIncludesLine index length start (end - 1) then
            ( content ++ newline ++ "\t" ++ line, index + 1 + length, indents + 1 )
        else
            ( content ++ newline ++ line, index + 1 + length, indents )

unindentBeforeCursor : Int -> String -> Bool
unindentBeforeCursor cursor =
    not << String.all ((==) '\t') << String.left (cursor + 1)

lineUnindentedIfSelected : Int -> Int -> String -> ( String, Int, Int, Bool ) -> ( String, Int, Int, Bool )
lineUnindentedIfSelected start end line ( content, index, unindents, startUnindented ) =
    let
        length = String.length line
        nextIndex = index + 1 + length
        newlineIfNotFirst = if index == 0 then "" else "\n"
        unindentable = String.startsWith "\t" line
        selectionStarted = withinRange start index (index + length)
        selectionEnded = withinRange end index (index + length)
        withinSelection = withinRange index start (end - 1)
    in
        if selectionStarted && selectionEnded && unindentable then
            let
                selectionStartInLine = start - index
                selectionEndInLine = end - index
                unindentBeforeStart = unindentBeforeCursor selectionStartInLine line
                unindentBeforeEnd = unindentBeforeCursor selectionEndInLine line
                unindentsBeforeEnd = if unindentBeforeEnd then 1 else 0
                unindentedLine = String.dropLeft 1 line
            in
                ( content ++ newlineIfNotFirst ++ unindentedLine, nextIndex, unindentsBeforeEnd, unindentBeforeStart )
        else if selectionStarted && unindentable then
            let
                selectionStartInLine = start - index
                unindentBeforeStart = unindentBeforeCursor selectionStartInLine line
                unindentedLine = String.dropLeft 1 line
            in
                ( content ++ newlineIfNotFirst ++ unindentedLine, nextIndex, 1, unindentBeforeStart )
        else if selectionEnded && unindentable && end /= index then
            let
                selectionEndInLine = end - index
                unindentBeforeEnd = unindentBeforeCursor selectionEndInLine line
                unindentsBeforeEnd = unindents + if unindentBeforeEnd then 1 else 0
                unindentedLine = String.dropLeft 1 line
            in
                ( content ++ newlineIfNotFirst ++ unindentedLine, nextIndex, unindentsBeforeEnd, startUnindented )
        else if withinSelection && unindentable then
            let
                unindentsBeforeEnd = unindents + 1
                unindentedLine = String.dropLeft 1 line
            in
                ( content ++ newlineIfNotFirst ++ unindentedLine, nextIndex, unindentsBeforeEnd, startUnindented )
        else
            ( content ++ newlineIfNotFirst ++ line, nextIndex, unindents, startUnindented )

contentIndented : Model -> Int -> Int -> Model
contentIndented model selectionStart selectionEnd =
    let
        ( content, _, indents ) =
            foldl
                ( lineIndentedIfSelected selectionStart selectionEnd )
                ( "", 0, 0 )
                ( String.lines model.content )
    in
        { model
            | content = content
            , selectionStart = selectionStart + 1
            , selectionEnd = selectionEnd + indents
        }

contentUnindented : Model -> Int -> Int -> Model
contentUnindented model selectionStart selectionEnd =
    let
        ( content, _, unindents, startUnindented ) =
            foldl
                ( lineUnindentedIfSelected selectionStart selectionEnd )
                ( "", 0, 0, False )
                ( String.lines model.content )
    in
        { model
            | content = content
            , selectionStart = selectionStart - ( if startUnindented then 1 else 0 )
            , selectionEnd = selectionEnd - unindents
        }

modelWithContentTabbed : Model -> Bool -> Int -> Int -> Model
modelWithContentTabbed model shiftPressed selectionStart selectionEnd =
    if shiftPressed then
        Debug.log "Unindent" (contentUnindented model selectionStart selectionEnd)
    else if newlineExists selectionStart selectionEnd model.content then
        contentIndented model selectionStart selectionEnd
    else
        modelWithChar model '\t' selectionStart selectionEnd

modelWithoutSelection : Model -> Int -> Int -> Model
modelWithoutSelection model selectionStart selectionEnd =
    { model
        | content = stringWithoutSelection model.content selectionStart selectionEnd
        , selectionStart = selectionStart
        , selectionEnd = selectionStart
    }

charPairs : Set String
charPairs =
    Set.fromList ["()", "{}", "[]", "\"\"", "''"]

betweenCharPair : String -> Int -> Bool
betweenCharPair string position =
    member (String.slice (position - 1) (position + 1) string) charPairs

modelWithContentBackspaced : Model -> Int -> Int -> Model
modelWithContentBackspaced model selectionStart selectionEnd =
    if selectionStart /= selectionEnd then
        modelWithoutSelection model selectionStart selectionEnd
    else if selectionStart == 0 then
        model
    else if betweenCharPair model.content selectionStart then
        modelWithoutSelection model (selectionStart - 1) (selectionStart + 1)
    else
        modelWithoutSelection model (selectionStart - 1) selectionStart

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpecialKeyPress keyboardEvent ->
            case keyboardEvent.key of
                "Backspace" ->
                    modelWithContentBackspaced model keyboardEvent.selectionStart keyboardEvent.selectionEnd ! []
                "Tab" ->
                    modelWithContentTabbed model keyboardEvent.shift keyboardEvent.selectionStart keyboardEvent.selectionEnd ! []
                {-"Enter" ->
                    { model | content = model.content ++ "\n" } ! []
                "(" ->
                    { model | content = model.content ++ "\n" } ! []
                ")" ->
                    { model | content = model.content ++ "\n" } ! []-}
                _ ->
                    model ! []
        KeyUp keyboardEvent ->
            Debug.log "KeyUp" { model | selectionStart = keyboardEvent.selectionStart, selectionEnd = keyboardEvent.selectionEnd } ! []
        Input content ->
            { model | content = content } ! []

specialKeys : Set String
specialKeys =
    Set.fromList ["Backspace", "Tab"]--, "Enter", "(", ")", "{", "}", "[", "]", "\"", "'"]

succeededIfSpecialKey : KeyboardEvent -> Decode.Decoder KeyboardEvent
succeededIfSpecialKey keyboardEvent =
    if member keyboardEvent.key specialKeys then
        Decode.succeed keyboardEvent
    else
        Decode.fail "non-tab"

type alias KeyboardEvent =
    { key : String
    , selectionStart : Int
    , selectionEnd : Int
    , shift : Bool
    }

keyboardEventDecoder : Decode.Decoder KeyboardEvent
keyboardEventDecoder =
    Decode.map4 KeyboardEvent
        ( Decode.field "key" Decode.string )
        ( Decode.at [ "target", "selectionStart" ] Decode.int )
        ( Decode.at [ "target", "selectionEnd" ] Decode.int )
        ( Decode.field "shiftKey" Decode.bool )

specialKeyPressed : Decode.Decoder Msg
specialKeyPressed =
    Decode.andThen succeededIfSpecialKey keyboardEventDecoder
        |> Decode.map SpecialKeyPress

view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ id "dt"
            , class "code"
            , cols 40
            , rows 20
            , onInput Input
            , onWithOptions "keydown" { defaultOptions | preventDefault = True } specialKeyPressed
            , on "keyup" <| Decode.map KeyUp keyboardEventDecoder
            , value model.content
            , property "selectionStart" <| Encode.int model.selectionStart
            , property "selectionEnd" <| Encode.int model.selectionEnd
            ] []
        ]