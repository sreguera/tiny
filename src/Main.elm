module Main exposing (..)

-- Tiny Basic interpreter.

import Browser
import Html exposing (Html, div, text, input, form, br, output)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (value)
import Ilvm exposing (VM, resume, resumeWithInput)
import Interp
import Html.Attributes exposing (style)

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
    div [style "font-family" "monospace"]
        [ div [ style "width" "64ch" ] (List.intersperse (br [] []) (List.map text (List.reverse model.log)))
        , form [ onSubmit GotReturn ] 
          [ input [ value model.inp, onInput GotInput, style "font-family" "monospace", style "width" "64ch" ] [] ]
        , div [] [ text "Lines:", text (Debug.toString model.vm.lines) ]
        , div [] [ text "Vars:", text (Debug.toString model.vm.vars) ]
        ]
