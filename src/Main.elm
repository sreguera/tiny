module Main exposing (..)

-- Tiny Basic interpreter.

import Browser
import Html exposing (Html, div, text, input, form)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (value)
import Array exposing (Array)
import Dict exposing (Dict)

-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Address = Int

type Opcode 
  = TST Address String
  | CALL Address         -- Execute the IL subroutine at address. Save next inst in control stack.
  | RTN                  -- Return from IL subroutine to the inst at the top of the control stack.
  | DONE
  | JMP Address          -- Continue execution at the address.
  | PRS
  | PRN
  | SPC
  | NLINE
  | NXT
  | XFER
  | SAV                  -- Push current line number onto sbrstk.
  | RSTR                 -- Replace current line number with top of sbrstk.
  | CMPR
  | LIT Int              -- Push the number onto the aestk.
  | INNUM
  | FIN                  -- Return to the line collect routine.
  | ERR
  | ADD                  -- Replace top two elements of aestk by their sum.
  | SUB                  -- Replace top two elements of aestk by their difference.
  | NEG                  -- Replace top of aestk with its negative.
  | MUL                  -- Replace top two elements of aestk by their product.
  | DIV                  -- Replace top two elements of aestk by their quotient.
  | STORE                -- Place the value at top of aestk at the var below it.
  | TSTV Address
  | TSTN Address
  | IND                  -- Replace top of aestk by the var it indexes.
  | LST
  | INIT
  | GETLINE              -- Input a line to lbuf.
  | TSTL Address
  | INSRT
  | XINIT

type alias VM =
  { pc : Address
  , code : Array Opcode
  , lbuf : String
  , aestk : List Int
  , cstk : List Address
  , vars : Dict Int Int
  , curline : Int
  , sbrstk : List Int
  }

initialVM : VM
initialVM =
  { pc = 0
  , code = Array.fromList [ INIT, NLINE, GETLINE, JMP 2 ]
  , lbuf = ""
  , aestk = []
  , cstk = []
  , vars = Dict.empty
  , curline = 0
  , sbrstk = []
  }

type Next
  = Cont
  | Stop

exec1 : VM -> (VM, Next)
exec1 vm =
  case Maybe.withDefault ERR (Array.get vm.pc vm.code) of
    INIT ->
      ( { vm | pc = vm.pc + 1 }, Cont )
    GETLINE ->
      ( { vm | pc = vm.pc + 1 }, Stop )
    FIN ->
      ( { vm | pc = 2 }, Cont )
    JMP addr ->
      ( { vm | pc = addr }, Cont )
    CALL addr ->
      ( { vm | pc = addr, cstk = vm.pc + 1 :: vm.cstk }, Cont )
    RTN ->
      case vm.cstk of
        a :: rest ->
          ( { vm | pc = a, cstk = rest }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    STORE ->
      case vm.aestk of
        a :: b :: rest ->
          ( { vm | pc = vm.pc + 1, aestk = rest, vars = Dict.insert b a vm.vars }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    IND ->
      case vm.aestk of
        a :: rest ->
          case Dict.get a vm.vars of
            Just v ->
              ( { vm | pc = vm.pc + 1, aestk = v :: rest }, Cont )
            _ ->
              ( vm, Cont ) -- TODO: Error 
        _ ->
          ( vm, Cont ) -- TODO: Error 
    LIT val ->
      ( { vm | pc = vm.pc + 1, aestk = val :: vm.aestk }, Cont )
    ADD ->
      case vm.aestk of
        a :: b :: rest ->
          ( { vm | pc = vm.pc + 1, aestk = a + b :: rest }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    SUB ->
      case vm.aestk of
        a :: b :: rest ->
          ( { vm | pc = vm.pc + 1, aestk = b - a :: rest }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    NEG ->
      case vm.aestk of
        a :: rest ->
          ( { vm | pc = vm.pc + 1, aestk =  -a :: rest }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    MUL ->
      case vm.aestk of
        a :: b :: rest ->
          ( { vm | pc = vm.pc + 1, aestk = a * b :: rest }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    DIV ->
      case vm.aestk of
        a :: b :: rest ->
          ( { vm | pc = vm.pc + 1, aestk = b // a :: rest }, Cont )
        _ ->
          ( vm, Cont ) -- TODO: Error 
    _ ->
      ( { vm | pc = vm.pc + 1 }, Cont )

execN : VM -> Int -> (VM, Next)
execN vm n =
  if n == 0 then
    ( vm, Cont )
  else
    case exec1 vm of
      ( vm1, Stop ) ->
        ( vm1, Stop)
      ( vm1, Cont ) ->
        execN vm1 (n - 1)

resume : VM -> VM
resume vm =
  case exec1 vm of
      ( vm1, Stop ) ->
        vm1
      ( vm1, Cont ) ->
        resume vm1

resumeWithInput : VM -> String -> VM
resumeWithInput vm s =
  resume { vm | lbuf = s }

type alias Model = 
  { vm : VM
  , log : String
  , inp : String
  }

init : Model
init =
  { vm = resume initialVM
  , log = ""
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
      { vm = resumeWithInput model.vm model.inp
      , log = model.log ++ "\n" ++ model.inp
      , inp = ""
      }
    _ ->
      model

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text model.log ] 
    , form [ onSubmit GotReturn ] 
      [ input [ value model.inp, onInput GotInput ] [] ]
    ]
