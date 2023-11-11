module Main exposing (..)

-- Tiny Basic interpreter.

import Browser
import Element exposing (Element, fill, rgb)
import Element.Input
import Element.Font as Font
import Element.Background as Background
import Html exposing (Html)
import Html.Events
import Ilvm exposing (VM, resume, resumeWithInput, break, Next(..))
import Interp
import Json.Decode as Decode
import Dict
import Array
import Time
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd
import Word

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

-- MODEL

type alias Model = 
    { vm : VM
    , log : List String
    , input : Input
    }

type Input
    = Chars String
    | Break

stringFromInput : Input -> String
stringFromInput input =
    case input of
        Chars s ->
            s
        Break ->
            ""

init : () -> (Model, Cmd Msg)
init _ =
    (   { vm = Interp.initialVM
        , log = []
        , input = Chars ""
        }
    , Cmd.none
    )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 20 Tick

-- UPDATE

type Msg
    = Tick Time.Posix
    | GotInput String
    | GotReturn
    | GotBreak

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotInput str ->
            ( { model | input = Chars str }, Cmd.none )
        GotBreak ->
            ( { model | input = Break }, Cmd.none )
        GotReturn ->
            case model.vm.next of
                Input _ ->
                    let
                        vm1 = resumeWithInput model.vm (stringFromInput model.input)
                        output = if String.isEmpty vm1.output then [] else (List.reverse (String.lines vm1.output))
                        model1 = 
                            { vm = { vm1 | output = "" }
                            , log = output ++ (stringFromInput model.input) :: model.log
                            , input = Chars ""
                            }
                    in
                    (model1, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Tick _ ->
            case model.vm.next of
                Continue ->
                    case model.input of
                        Break ->
                            let
                                vm1 = break model.vm
                                output = if String.isEmpty vm1.output then [] else (List.reverse (String.lines vm1.output))
                                model1 =
                                    { vm = { vm1 | output = "" }
                                    , log = output ++ model.log
                                    , input = Chars ""
                                    }
                            in
                            (model1, Cmd.none)
                        _ ->
                            let
                                vm1 = resume model.vm
                                output = if String.isEmpty vm1.output then [] else (List.reverse (String.lines vm1.output))
                                model1 = 
                                    { vm = { vm1 | output = "" }
                                    , log = output ++ model.log
                                    , input = model.input
                                    }
                            in
                            (model1, Cmd.none)
                _ ->
                    (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    Element.layout []
        (viewModel model)

viewModel : Model -> Element Msg
viewModel model =
    Element.column []
        [ Element.text "Tiny Basic"
        , Element.row []
            [ Element.column [ Element.width fill, Element.alignTop ]
                [ viewTerminal model
                , viewLines model
                , viewVars model
                ]
            , viewUcode model
            ]
        ]

-- From elm-ui examples
onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )

viewTerminal : Model -> Element Msg
viewTerminal model =
    Element.column [ Font.family [Font.monospace] ]
        [ viewLog model.log
        , Element.Input.text [ onEnter GotReturn ] 
            { onChange = GotInput
            , text = stringFromInput model.input
            , placeholder = Nothing
            , label = Element.Input.labelHidden ""
            }
        , Element.Input.button []
            { onPress = Just GotBreak
            , label = Element.text "Break"
            }
        ]

viewLog : List String -> Element Msg
viewLog log =
    Element.column []
        (List.map Element.text (List.reverse log))

viewLines : Model -> Element Msg
viewLines model =
    let
        showCurrent lineno elem =
            if lineno == model.vm.curline then
                Element.el [ Background.color (rgb 0.7 0.7 0.9) ] elem
            else
                elem
    in
    Element.table [ Font.family [Font.monospace] ]
        { data = Dict.toList model.vm.lines
        , columns =
            [ { header = Element.text "Lineno"
              , width = Element.fill
              , view = \(lineno, _) -> showCurrent lineno <| Element.text (String.fromInt lineno)
              }
            , { header = Element.text "Code"
              , width = Element.fill
              , view = \(lineno, code) -> showCurrent lineno <| Element.text code
              }
            ]
        }

viewVars : Model -> Element Msg
viewVars model =
    Element.table [ Font.family [Font.monospace] ]
        { data = Dict.toList model.vm.vars
        , columns =
            [ { header = Element.text "Var"
              , width = Element.fill
              , view = \(var, _) -> Element.text (String.fromChar (Char.fromCode (var + Char.toCode 'A')))
              }
            , { header = Element.text "Value"
              , width = Element.fill
              , view = \(_, value) -> Element.text (String.fromInt (Word.toInt value))
              }
            ]
        }

viewUcode : Model -> Element Msg
viewUcode model =
    let
        showCurrent address elem =
            if address == model.vm.pc then
                Element.el [ Background.color (rgb 0.7 0.7 0.9) ] elem
            else
                elem
    in
    Element.table [ Font.family [Font.monospace], Element.scrollbarY, Element.height (Element.px 500) ]
        { data = Array.toIndexedList model.vm.code
        , columns =
            [ { header = Element.text "Address"
              , width = Element.fill
              , view = \(address, _) -> showCurrent address <| Element.text (String.fromInt address)
              }
            , { header = Element.text "Opcode"
              , width = Element.fill
              , view = \(address, opcode) -> showCurrent address <| Element.text (Debug.toString opcode)
              }
            ]
        }

