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


evaluate : Instruction -> Evaluation -> ( Evaluation, Int )
evaluate instruction ({ registers, tone } as evaluation) =
    let
        getRegister register =
            Dict.get register registers

        getValue value =
            case value of
                Register register ->
                    getRegister register

                Number number ->
                    Just number

        update f register value =
            Maybe.map2
                (\a b ->
                    { evaluation
                        | registers =
                            Dict.insert register (f a b) registers
                    }
                )
                (getRegister register)
                (getValue value)
                |> Maybe.withDefault evaluation

        safeModulo a b =
            if b == 0 then
                a
            else
                a % b

        jumpOne result =
            ( result, 1 )
    in
    case instruction of
        Isnd register ->
            getRegister register
                |> Maybe.map
                    (\number ->
                        { evaluation | tone = Playing number }
                    )
                |> Maybe.withDefault evaluation
                |> jumpOne

        Iset register value ->
            getValue value
                |> Maybe.map
                    (\number ->
                        { evaluation
                            | registers = Dict.insert register number registers
                        }
                    )
                |> Maybe.withDefault evaluation
                |> jumpOne

        Iadd register value ->
            update (+) register value
                |> jumpOne

        Imul register value ->
            update (*) register value
                |> jumpOne

        Imod register value ->
            update safeModulo register value
                |> jumpOne

        Ircv register ->
            getRegister register
                |> Maybe.map
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
                |> Maybe.withDefault evaluation
                |> jumpOne

        Ijgz register value ->
            Maybe.map2
                (\registerValue jumpValue ->
                    if registerValue > 0 then
                        ( evaluation, jumpValue )
                    else
                        ( evaluation, 1 )
                )
                (getRegister register)
                (getValue value)
                |> Maybe.withDefault (jumpOne evaluation)


evaluateFromList :
    List Instruction
    -> ( Evaluation, Int )
    -> Maybe ( Evaluation, Int )
evaluateFromList instructions ( evaluation, pos ) =
    List.Extra.getAt pos instructions
        |> Maybe.map (flip evaluate evaluation >> Tuple.mapSecond ((+) pos))


evaluateN : Int -> List Instruction -> Maybe ( Evaluation, Int )
evaluateN n instructions =
    List.foldl
        (always (Maybe.andThen (evaluateFromList instructions)))
        (Just ( emptyEvaluation, 0 ))
        (List.repeat n ())


firstRecovered : List Instruction -> Maybe Int
firstRecovered instructions =
    firstRecoveredHelper instructions ( emptyEvaluation, 0 )


firstRecoveredHelper :
    List Instruction
    -> ( Evaluation, Int )
    -> Maybe Int
firstRecoveredHelper instructions ( evaluation, pos ) =
    case evaluation.tone of
        Recovering number ->
            Just number

        _ ->
            evaluateFromList instructions ( evaluation, pos )
                |> Maybe.andThen (firstRecoveredHelper instructions)


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
