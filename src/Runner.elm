port module Runner exposing (..)

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Json.Decode


type alias Model =
    ()


type Msg
    = Input String


port input : (String -> msg) -> Sub msg


port output : ( Bool, String ) -> Cmd msg


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subcriptions
        }


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


subcriptions : Model -> Sub Msg
subcriptions model =
    input Input


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input day ->
            case run day of
                Ok ( result1, result2 ) ->
                    ( (), output ( True, "Result 1: " ++ result1 ++ "\nResult 2: " ++ result2 ) )

                Err message ->
                    ( (), output ( False, message ) )


run : String -> Result String ( String, String )
run day =
    case day of
        "1" ->
            Ok (Day1.output ())

        "2" ->
            Ok (Day2.output ())

        "3" ->
            Ok (Day3.output ())

        "4" ->
            Ok (Day4.output ())

        "5" ->
            Ok (Day5.output ())

        "6" ->
            Ok (Day6.output ())

        "7" ->
            Ok (Day7.output ())

        "8" ->
            Ok (Day8.output ())

        "9" ->
            Ok (Day9.output ())

        "10" ->
            Ok (Day10.output ())

        "11" ->
            Ok (Day11.output ())

        "12" ->
            Ok (Day12.output ())

        "13" ->
            Ok (Day13.output ())

        "14" ->
            Ok (Day14.output ())

        "15" ->
            Ok (Day15.output ())

        "16" ->
            Ok (Day16.output ())

        "17" ->
            Ok (Day17.output ())

        "18" ->
            Ok (Day18.output ())

        "19" ->
            Ok (Day19.output ())

        "20" ->
            Ok (Day20.output ())

        _ ->
            Err ("Invalid day value. See the end of src/Runner.elm for valid ones. Got: " ++ day)
