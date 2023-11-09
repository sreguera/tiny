module Main exposing (..)

-- Tiny Basic interpreter.

import Browser
import Element exposing (Element, fill, rgb)
import Element.Input
import Element.Font as Font
import Element.Background as Background
import Html exposing (Html)
import Html.Events
import Ilvm exposing (VM, resume, resumeWithInput, Next(..))
import Interp
import Json.Decode as Decode
import Dict
import Array
import Time
import Platform.Cmd as Cmd

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
    , inp : String
    }

init : () -> (Model, Cmd Msg)
init _ =
    (   { vm = resume Interp.initialVM
        , log = []
        , inp = ""
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotInput str ->
            ( { model | inp = str }, Cmd.none )
        GotReturn ->
            let
                vm1 = resumeWithInput model.vm model.inp
                output = if String.isEmpty vm1.output then [] else (List.reverse (String.lines vm1.output))
                model1 = 
                    { vm = { vm1 | output = "" }
                    , log = output ++ model.inp :: model.log
                    , inp = ""
                    }
            in
            (model1, Cmd.none)
        Tick _ ->
            case model.vm.nextAction of
                Stop ->
                    (model, Cmd.none)
                Cont ->
                    let
                        vm1 = resume model.vm
                        output = if String.isEmpty vm1.output then [] else (List.reverse (String.lines vm1.output))
                        model1 = 
                            { vm = { vm1 | output = "" }
                            , log = output ++ model.log
                            , inp = ""
                            }
                    in
                    (model1, Cmd.none)

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
            , text = model.inp
            , placeholder = Nothing
            , label = Element.Input.labelHidden ""
            }
        ]

viewLog : List String -> Element Msg
viewLog log =
    Element.column []
        (List.map Element.text (List.reverse log))

viewLines : Model -> Element Msg
viewLines model =
    Element.table [ Font.family [Font.monospace] ]
        { data = Dict.toList model.vm.lines
        , columns =
            [ { header = Element.text "Lineno"
              , width = Element.fill
              , view = \(lineno, _) -> Element.text (String.fromInt lineno)
              }
            , { header = Element.text "Code"
              , width = Element.fill
              , view = \(_, code) -> Element.text code
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
              , view = \(_, value) -> Element.text (String.fromInt value)
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

