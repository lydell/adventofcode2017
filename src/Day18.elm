module Day18 exposing (..)

import Dict exposing (Dict)
import List.Extra


output : () -> ( String, String )
output () =
    ( input |> parse |> firstRecovered |> toString
    , ""
    )


type Instruction
    = Isnd String
    | Iset String Value
    | Iadd String Value
    | Imul String Value
    | Imod String Value
    | Ircv String
    | Ijgz String Value


type Value
    = Register String
    | Number Int


type RuntimeError
    = ReferenceError String Instruction
    | ZeroDivisionError Instruction


type Tone
    = Silent
    | Playing Int
    | Recovering Int


type alias Evaluation =
    { registers : Dict String Int
    , tone : Tone
    }


parse : String -> List Instruction
parse =
    String.lines
        >> List.filterMap (parseLine >> Result.toMaybe)


parseLine : String -> Result String Instruction
parseLine string =
    case String.words string of
        [ "snd", register ] ->
            Ok <| Isnd register

        [ "set", register, value ] ->
            Ok <| Iset register (parseValue value)

        [ "add", register, value ] ->
            Ok <| Iadd register (parseValue value)

        [ "mul", register, value ] ->
            Ok <| Imul register (parseValue value)

        [ "mod", register, value ] ->
            Ok <| Imod register (parseValue value)

        [ "rcv", register ] ->
            Ok <| Ircv register

        [ "jgz", register, value ] ->
            Ok <| Ijgz register (parseValue value)

        _ ->
            Err <| "Unknown/invalid operation: " ++ string


parseValue : String -> Value
parseValue string =
    case String.toInt string of
        Ok number ->
            Number number

        Err _ ->
            Register string


emptyEvaluation : Evaluation
emptyEvaluation =
    { registers = Dict.empty, tone = Silent }


evaluate : Instruction -> Evaluation -> Result RuntimeError ( Evaluation, Int )
evaluate instruction ({ registers, tone } as evaluation) =
    let
        getRegister register =
            case Dict.get register registers of
                Just number ->
                    Ok number

                Nothing ->
                    Err <| ReferenceError register instruction

        getValue value =
            case value of
                Register register ->
                    getRegister register

                Number number ->
                    Ok number

        update f register value =
            Result.map2
                (\a b ->
                    f a b
                        |> Result.map
                            (\number ->
                                { evaluation
                                    | registers =
                                        Dict.insert register number registers
                                }
                            )
                )
                (getRegister register)
                (getValue value)
                |> Result.andThen identity

        ok f a b =
            f a b |> Ok

        safeModulo instruction a b =
            if b == 0 then
                Err <| ZeroDivisionError instruction
            else
                Ok <| a % b

        jumpOne =
            Result.map (\result -> ( result, 1 ))
    in
    case instruction of
        Isnd register ->
            getRegister register
                |> Result.map
                    (\number ->
                        { evaluation | tone = Playing number }
                    )
                |> jumpOne

        Iset register value ->
            getValue value
                |> Result.map
                    (\number ->
                        { evaluation
                            | registers = Dict.insert register number registers
                        }
                    )
                |> jumpOne

        Iadd register value ->
            update (ok (+)) register value
                |> jumpOne

        Imul register value ->
            update (ok (*)) register value
                |> jumpOne

        Imod register value ->
            update (safeModulo instruction) register value
                |> jumpOne

        Ircv register ->
            getRegister register
                |> Result.map
                    (\number ->
                        if number == 0 then
                            evaluation
                        else
                            case tone of
                                Silent ->
                                    evaluation

                                Playing playingNumber ->
                                    { evaluation
                                        | tone = Recovering playingNumber
                                    }

                                Recovering _ ->
                                    evaluation
                    )
                |> jumpOne

        Ijgz register value ->
            Result.map2
                (\registerValue jumpValue ->
                    if registerValue > 0 then
                        ( evaluation, jumpValue )
                    else
                        ( evaluation, 1 )
                )
                (getRegister register)
                (getValue value)


firstRecovered : List Instruction -> Result RuntimeError (Maybe Int)
firstRecovered instructions =
    firstRecoveredHelper instructions 0 emptyEvaluation


firstRecoveredHelper :
    List Instruction
    -> Int
    -> Evaluation
    -> Result RuntimeError (Maybe Int)
firstRecoveredHelper instructions pos evaluation =
    case evaluation.tone of
        Recovering number ->
            Ok (Just number)

        _ ->
            case List.Extra.getAt pos instructions of
                Just instruction ->
                    evaluate instruction evaluation
                        |> Result.andThen
                            (\( newEvaluation, jump ) ->
                                firstRecoveredHelper
                                    instructions
                                    (pos + jump)
                                    newEvaluation
                            )

                Nothing ->
                    Ok Nothing


input : String
input =
    """set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 622
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19"""
