module Main exposing (..)

-- Tiny Basic interpreter.

import Browser
import Element exposing (Element)
import Element.Input
import Html exposing (Html)
import Ilvm exposing (VM, resume, resumeWithInput)
import Interp

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
    { vm : VM
    , log : List String
    , inp : String
    }

init : Model
init =
    { vm = resume Interp.initialVM
    , log = []
    , inp = ""
    }

-- UPDATE

type Msg
    = Init
    | GotInput String
    | GotReturn

update : Msg -> Model -> Model
update msg model =
    case msg of
        GotInput str ->
            { model | inp = str } 
        GotReturn ->
            let
                vm1 = resumeWithInput model.vm model.inp
                output = if String.isEmpty vm1.output then [] else (List.reverse (String.lines vm1.output))
            in
            { vm = { vm1 | output = "" }
            , log = output ++ model.inp :: model.log
            , inp = ""
            }
        _ ->
            model

-- VIEW

view : Model -> Html Msg
view model =
    Element.layout []
        (viewModel model)

viewModel : Model -> Element Msg
viewModel model =
    Element.column []
        [ Element.text "Tiny Basic"
        , viewTerminal model
        ]

viewTerminal : Model -> Element Msg
viewTerminal model =
    Element.column []
        [ viewLog model.log
        , Element.Input.text [] 
            { onChange = GotInput
            , text = model.inp
            , placeholder = Nothing
            , label = Element.Input.labelHidden ""
            }
        , Element.Input.button []
            { onPress = Just GotReturn
            , label = Element.text "CR"
            }
        ]

viewLog : List String -> Element Msg
viewLog log =
    Element.column []
        (List.map Element.text (List.reverse log))
