module Day8 exposing (..)

import Dict exposing (Dict)


output : () -> ( String, String )
output () =
    ( input |> parse |> evaluate |> largestRegisterValue |> toString
    , input |> parse |> evaluate |> .largestValue |> toString
    )


type alias Instruction =
    { register : String
    , changeType : ChangeType
    , changeValue : Int
    , condition : Condition
    }


type ChangeType
    = Inc
    | Dec


type alias Condition =
    { operator : Operator
    , register : String
    , value : Int
    }


type Operator
    = O_LT
    | O_LTE
    | O_GT
    | O_GTE
    | O_EQ
    | O_NEQ


type alias Registry =
    { registers : Dict String Int
    , largestValue : Int
    }


parse : String -> List Instruction
parse string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe Instruction
parseLine string =
    case String.words string of
        [ register, changeTypeString, changeValueString, ifKeyword, conditionRegister, operatorString, conditionValueString ] ->
            let
                parsed =
                    ( parseChangeType changeTypeString
                    , String.toInt changeValueString
                    , parseOperator operatorString
                    , String.toInt conditionValueString
                    )
            in
            case parsed of
                ( Ok changeType, Ok changeValue, Ok operator, Ok conditionValue ) ->
                    Just
                        { register = register
                        , changeType = changeType
                        , changeValue = changeValue
                        , condition =
                            { operator = operator
                            , register = conditionRegister
                            , value = conditionValue
                            }
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


parseChangeType : String -> Result String ChangeType
parseChangeType string =
    case string of
        "inc" ->
            Ok Inc

        "dec" ->
            Ok Dec

        _ ->
            Err ("Invalid change type: " ++ string)


parseOperator : String -> Result String Operator
parseOperator string =
    case string of
        "<" ->
            Ok O_LT

        "<=" ->
            Ok O_LTE

        ">" ->
            Ok O_GT

        ">=" ->
            Ok O_GTE

        "==" ->
            Ok O_EQ

        "!=" ->
            Ok O_NEQ

        _ ->
            Err ("Invalid operator: " ++ string)


evaluate : List Instruction -> Registry
evaluate instructions =
    List.foldl evaluateInstruction { registers = Dict.empty, largestValue = 0 } instructions


evaluateInstruction : Instruction -> Registry -> Registry
evaluateInstruction instruction registry =
    let
        { registers, largestValue } =
            registry

        sign =
            case instruction.changeType of
                Inc ->
                    1

                Dec ->
                    -1

        increment =
            instruction.changeValue * sign

        shouldUpdate =
            evaluateCondition instruction.condition registers

        oldValue =
            Dict.get instruction.register registers
                |> Maybe.withDefault 0

        newValue =
            oldValue + increment
    in
    if shouldUpdate then
        { registers = Dict.insert instruction.register newValue registers
        , largestValue = max largestValue newValue
        }
    else
        registry


evaluateCondition : Condition -> Dict String Int -> Bool
evaluateCondition condition registers =
    let
        registerValue =
            Dict.get condition.register registers
                |> Maybe.withDefault 0

        operatorFunction =
            getOperatorFunction condition.operator
    in
    operatorFunction registerValue condition.value


getOperatorFunction : Operator -> (comparable -> comparable -> Bool)
getOperatorFunction operator =
    case operator of
        O_LT ->
            (<)

        O_LTE ->
            (<=)

        O_GT ->
            (>)

        O_GTE ->
            (>=)

        O_EQ ->
            (==)

        O_NEQ ->
            (/=)


largestRegisterValue : Registry -> Maybe Int
largestRegisterValue =
    .registers >> Dict.values >> List.maximum


input : String
input =
    """y inc 497 if n <= 3
ig inc -54 if es < 9
j dec 278 if low < 10
nm inc -531 if tr == 0
tq inc 537 if tq < 9
txm dec 835 if s != -8
xho dec -204 if vv < 6
ipq dec 59 if txm != -835
vv dec -259 if xho <= 210
tq inc 364 if qen != -7
xho inc -198 if xho < 205
afo dec -182 if j == -278
qen inc 774 if ntk == 4
cwp dec -414 if tr < 4
vv dec -696 if ntk == 0
low dec 851 if n == 0
tq inc -132 if t > -10
mux dec 626 if j >= -283
wby dec 759 if vv >= 950
qen inc 853 if j >= -283
qen inc -571 if tr != 0
ntk inc 213 if n == 0
qen dec 714 if qen == 853
es inc -936 if qen == 139
tq inc -496 if vv >= 955
tr dec -242 if afo > 175
tr inc 937 if ipq == -8
es inc 543 if wby <= -752
s dec -670 if low >= -860
vv inc 858 if ig == -46
ixp inc 292 if zhz >= -5
zhz dec 628 if xho != 8
wby dec 538 if tr == 242
y dec -183 if j > -287
ig inc -804 if mux >= -633
afo inc -89 if ig >= -862
low dec -423 if vv == 951
vv inc 144 if vv == 955
cwp inc 317 if s <= 677
ipq dec 963 if cwp < 731
mux dec -663 if vv <= 1091
qen inc 522 if ipq == 0
nm inc 940 if t >= -7
low inc -847 if ixp != 290
ig inc 112 if t < 2
tr dec -910 if ntk == 213
afo dec 445 if nm == 409
zhz inc 93 if tq < 279
s dec 902 if jaq <= 2
ixp inc -250 if n != -4
mux inc 41 if es < -385
ntk dec -326 if vv < 1102
wby inc -735 if mux == -585
jaq dec -794 if afo >= -352
tr inc -212 if n > -7
y inc 132 if tr < 948
tq dec -72 if cwp <= 738
wby inc -154 if txm > -840
jaq dec -138 if xho != 4
ixp inc 201 if cwp > 729
tq dec 322 if wby < -2185
ig inc 333 if afo <= -344
nm dec 688 if tq < 31
nm dec 204 if jaq != 931
ig inc -650 if qen > 658
t inc -269 if ixp != 239
vv inc 925 if vv > 1096
ntk inc -763 if ig != -1054
t dec 50 if ipq < 8
es inc 437 if tr <= 947
wby dec -465 if s > -240
tq inc 374 if mux == -585
ipq inc 26 if afo >= -355
cwp dec 507 if j != -272
j inc -521 if ntk >= -229
wby inc 822 if jaq == 932
n dec 782 if low >= -1702
y dec -376 if nm != -481
y inc 402 if cwp == 224
zhz dec -396 if j >= -807
txm inc 679 if n >= -789
xho inc -680 if mux <= -585
ig dec 716 if tq != 397
n inc -392 if nm > -479
xho inc -142 if j <= -801
jaq dec 33 if tr < 942
y dec 822 if cwp <= 228
jaq dec 554 if low >= -1698
low dec -208 if s >= -239
ntk inc 219 if es == 44
mux dec 895 if jaq > 335
es inc 832 if ntk < 0
vv dec 68 if cwp > 231
cwp dec -357 if y <= 776
es dec 969 if es < 886
nm dec 268 if txm <= -158
vv dec 702 if ipq > 24
tr dec -150 if mux > -1482
cwp dec -591 if low != -1483
cwp dec -404 if tr >= 1089
mux inc -586 if ipq > 19
xho inc -80 if n >= -784
tr dec -933 if cwp >= 1575
qen inc 512 if txm >= -160
n inc 206 if low > -1493
ixp inc -157 if n == -576
es inc -285 if s < -222
j dec -594 if qen >= 1164
xho inc -7 if es == -380
afo dec -475 if tq <= 406
ixp dec -542 if ixp == 93
xho dec 394 if zhz == -144
xho inc 263 if afo >= 121
zhz dec 393 if low <= -1482
xho inc -100 if j >= -212
ntk dec 748 if jaq == 345
nm inc -477 if y <= 770
mux inc -256 if xho == -591
j dec 698 if low != -1490
vv dec 857 if jaq == 345
ntk inc -133 if mux >= -2331
mux inc -570 if tr <= 2028
t dec 35 if ntk <= -886
ixp inc 437 if j != -207
ipq inc -200 if txm < -156
j inc -968 if ig == -1063
j inc -250 if n > -580
zhz inc 794 if zhz <= -527
vv dec -413 if cwp > 1569
nm dec -698 if ntk != -878
jaq inc -937 if vv <= 884
afo dec -121 if ig < -1071
low inc -887 if cwp >= 1576
nm inc -290 if n != -577
vv dec -13 if t != -354
tq dec 240 if n != -576
y inc -173 if j > -1428
xho inc -348 if ipq <= 20
wby inc -385 if ig <= -1055
ntk dec 570 if txm < -154
j inc -55 if qen < 1182
xho dec 9 if txm <= -148
s inc 212 if afo != 118
low dec 51 if wby <= -1292
xho inc -81 if zhz != 262
zhz dec 190 if s != -12
low inc 475 if tr >= 2032
zhz dec -876 if txm < -164
txm inc 792 if tr <= 2030
zhz inc 643 if s != -22
afo inc 594 if nm < -542
y inc 31 if tr < 2014
xho inc 435 if qen > 1179
y dec -512 if tq == 397
y dec -186 if wby < -1278
t inc -194 if zhz != 718
tr dec 629 if y < 1295
xho dec -804 if jaq > -601
tq dec -581 if wby >= -1285
nm inc -172 if tr <= 1397
jaq dec 579 if vv < 876
vv dec -666 if ntk != -1456
tq dec 795 if cwp <= 1585
tq dec -62 if ntk >= -1464
vv dec 779 if qen >= 1171
ntk inc 815 if tq > 249
zhz dec 11 if ntk == -1456
xho dec 765 if j > -1480
zhz dec -694 if cwp <= 1583
low inc 795 if tq <= 248
n inc 756 if ntk != -1464
cwp inc -300 if t > -551
ipq dec 858 if xho > -564
ixp inc 975 if n >= 172
mux inc 954 if ixp > 1489
zhz inc 552 if qen >= 1169
ixp dec -398 if txm <= 642
wby inc -643 if ixp > 1886
y dec -978 if vv >= 95
mux inc 853 if nm <= -720
low inc -886 if mux > -1086
zhz dec -990 if jaq != -587
low inc 111 if es > -379
cwp dec 416 if y <= 2273
zhz dec 742 if ntk < -1452
es inc -655 if es == -378
vv inc 81 if wby > -1932
tr inc -572 if tr == 1394
qen dec -217 if wby > -1928
ig dec -562 if y >= 2269
qen dec 422 if wby < -1920
ig dec 146 if nm >= -730
afo inc -5 if txm < 638
qen dec -747 if j == -1478
ixp dec -197 if ipq == -832
tq dec 63 if mux <= -1093
tr dec -194 if y <= 2266
qen inc -894 if nm != -731
tr dec -258 if es < -1028
mux inc 290 if s >= -25
n inc 653 if tr == 1080
xho dec -724 if jaq == -584
wby inc -941 if ixp >= 2086
j inc -918 if j > -1471
es dec -179 if zhz != 2198
txm inc -726 if tq <= 251
j dec 875 if ntk <= -1448
tr inc -499 if ig > -642
j dec -440 if cwp != 853
ipq inc -325 if zhz >= 2197
mux dec 200 if j > -1921
vv inc -51 if es <= -1026
vv dec -96 if n != 833
t inc 598 if ntk > -1457
jaq inc -465 if vv >= 129
jaq dec -642 if n <= 830
mux dec 752 if nm > -727
xho inc -607 if nm < -717
xho inc 453 if jaq < -1060
mux inc -465 if vv > 122
xho inc -669 if vv < 135
tr inc -789 if vv > 121
low dec 65 if zhz >= 2204
low dec -161 if mux >= -2210
tr dec -559 if nm < -719
tq inc -414 if ipq != -1157
tr inc -290 if tq <= 247
low inc 435 if cwp == 860
vv dec -621 if cwp >= 853
ig inc -488 if t >= 50
wby dec 849 if ixp == 2093
xho dec -948 if mux < -2206
ipq inc 78 if mux != -2211
qen dec -912 if ipq > -1080
ixp dec -49 if ntk > -1459
qen inc 882 if j == -1913
txm dec 426 if qen == 2615
n dec -105 if txm <= -510
y dec -757 if tq == 235
y dec 786 if j < -1906
xho inc 283 if y != 1485
vv inc 346 if j != -1912
afo inc -849 if nm == -724
j inc -270 if n > 937
n dec -907 if txm == -520
wby inc -682 if ixp <= 2140
vv dec -617 if xho <= -886
y dec -97 if afo <= -136
wby inc 762 if ntk != -1455
es dec -39 if afo >= -143
afo inc 91 if tq == 245
vv inc -974 if xho >= -892
j dec 758 if afo == -46
ig dec 129 if es == -994
y inc 947 if tq < 246
qen dec -15 if cwp < 868
es inc -430 if zhz != 2193
j dec 132 if ntk != -1456
zhz inc -779 if y == 2529
cwp dec -382 if cwp == 866
mux inc -872 if jaq <= -1058
ixp inc -321 if ig >= -1273
tr inc -56 if ixp > 1814
wby inc -250 if wby >= -2955
es dec 771 if ntk < -1448
tq inc 830 if s > -25
ixp dec 255 if qen >= 2629
y inc -322 if mux != -2215
ipq inc -154 if xho == -889
wby dec -975 if ig >= -1267
zhz inc 159 if mux <= -2209
nm dec 178 if vv > 735
xho dec 998 if ntk != -1464
ig inc 106 if mux == -2212
jaq dec -414 if cwp >= 856
ixp inc 139 if low == -1922
ixp dec 283 if cwp <= 869
ixp dec -457 if wby <= -2229
low dec 684 if mux <= -2205
afo inc -823 if ntk <= -1451
n dec 979 if j == -2939
j dec 185 if mux <= -2204
mux dec 794 if vv <= 731
low inc -240 if j != -3130
ixp dec -563 if wby > -2238
mux inc -791 if ig >= -1161
txm dec -765 if ntk == -1456
ipq inc 570 if tq == 1075
j dec 341 if ntk == -1456
afo inc -960 if tr != 496
y inc -354 if t == 50
wby inc -461 if ipq > -656
mux dec 968 if jaq < -636
wby inc 535 if afo <= -1826
wby inc -140 if nm > -909
zhz inc -247 if es < -2194
es dec 832 if afo >= -1819
cwp dec -172 if ig > -1160
ntk dec 767 if wby >= -1841
afo dec 999 if s < -16
txm inc 562 if mux > -3972
zhz inc -463 if tq > 1069
zhz dec 628 if wby < -1830
vv dec -217 if j < -3474
s inc -201 if tq < 1066
txm inc -972 if es >= -2203
tq dec 720 if xho < -1879
t inc 268 if wby < -1831
cwp inc -213 if y < 1859
afo inc 674 if txm == -161
n inc 360 if ixp == 2442
tq inc -328 if cwp <= 822
y inc -500 if low <= -2856
ipq inc 650 if qen != 2639
low inc -635 if ixp <= 2434
ipq inc -281 if tr > 498
txm dec -111 if wby >= -1838
ixp dec -128 if es == -2195
tr dec -297 if y <= 1852
t inc 65 if ntk <= -2223
nm inc 999 if nm > -912
ixp inc 647 if ipq != -289
jaq inc -884 if afo > -2150
txm dec -505 if tr != 508
zhz inc -728 if es > -2197
cwp inc 578 if nm < 103
ipq inc 156 if tr > 497
es inc -500 if s == -20
j dec 296 if zhz <= -496
vv dec 524 if txm > 452
ipq inc 417 if tq != 31
tq dec 630 if cwp != 1404
jaq inc 791 if zhz == -496
cwp inc 5 if wby >= -1836
mux dec -502 if afo < -2147
n inc -296 if n >= 1306
vv dec 883 if mux < -3468
txm dec -593 if j >= -3471
j dec -34 if zhz < -487
ig dec 183 if ntk <= -2219
y dec 236 if j >= -3441
ntk inc 78 if mux == -3469
t inc -480 if xho == -1887
nm inc -337 if y < 1622
zhz inc 966 if j == -3433
y inc -46 if zhz < 481
qen inc 281 if vv == -668
jaq dec 324 if tq < -599
vv inc 882 if low == -2840
ntk inc -359 if txm < 1058
n dec -894 if low == -2846
n dec 762 if y >= 1564
txm inc 428 if afo < -2149
t inc 500 if ntk > -2500
ntk dec -175 if y < 1577
txm inc -851 if nm < -234
t dec -802 if mux == -3469
tr inc 254 if n != 1431
afo inc 910 if mux >= -3472
txm dec -99 if j < -3432
vv dec -689 if wby == -1835
es inc -446 if es < -2688
ixp inc 703 if afo != -1242
nm inc -260 if afo < -1240
afo dec -656 if wby >= -1836
afo inc 866 if s > -26
jaq inc 700 if ipq != 285
xho inc 770 if tq != -596
qen dec -878 if t < 709
txm inc 175 if ntk < -2327
tr dec -92 if ipq != 279
tq inc -406 if zhz < 485
ntk dec 19 if y < 1577
es inc 164 if vv < 23
j dec 341 if jaq == -267
txm dec -776 if j > -3784
cwp dec -22 if s >= -28
n dec 944 if jaq > -269
tq dec 658 if s > -23
tr inc -482 if t >= 696
cwp inc 66 if xho > -1126
es dec 880 if afo != 271
nm dec -457 if cwp >= 1491
ixp inc -690 if ntk < -2344
n inc 552 if jaq != -267
ntk dec 24 if tr == 276
low dec 582 if tr > 278
n dec -373 if j != -3776
wby inc 783 if ipq == 279
xho inc -720 if qen >= 3786
ntk inc 783 if xho < -1830
tq dec 720 if tq < -1675
vv inc -358 if wby > -1060
zhz inc 347 if tr < 278
vv inc -448 if n == 859
j dec 487 if ig >= -1342
n inc -604 if xho > -1840
ipq dec 898 if tq <= -1660
tq inc 229 if tr != 286
vv dec 21 if ixp < 3231
tq inc 747 if ixp > 3229
xho dec 48 if wby >= -1057
tq inc 364 if jaq >= -268
tq inc -647 if mux > -3462
tq inc -580 if j >= -4260
n dec 937 if tq <= -324
cwp inc -976 if xho < -1876
vv inc 555 if mux == -3469
txm inc 305 if wby == -1047
jaq inc -790 if qen < 3798
qen dec -83 if ntk < -1580
afo dec 725 if txm <= 1679
qen inc 790 if ixp <= 3223
jaq dec -693 if ixp <= 3223
tr inc 706 if txm == 1675
tq inc 547 if es < -3849
tq dec 841 if t < 706
j dec 393 if jaq >= -1063
tr dec 775 if ntk <= -1584
y inc -706 if mux >= -3464
cwp dec 436 if ixp == 3230
low dec -311 if xho >= -1886
t inc 777 if es >= -3858
t dec -916 if ipq >= -622
ipq inc 85 if xho >= -1885
nm inc 949 if xho == -1885
es dec -238 if j == -4663
mux inc 310 if tr == 207
tq dec 638 if cwp >= 76
mux inc -922 if ixp != 3233
txm inc 941 if cwp != 83
tq inc 646 if mux > -4086
qen inc -179 if xho >= -1878
zhz inc 359 if zhz != 825
ipq inc 173 if y >= 1563
txm dec -586 if s == -16
afo inc -614 if tr < 211
jaq inc 666 if ixp != 3230
ixp dec 153 if es >= -3858
nm dec 488 if j == -4654
t inc 660 if afo != -1056
ipq inc -447 if tr >= 200
cwp dec 883 if t < 3062
txm inc -846 if tr > 198
j dec -33 if ipq < -815
zhz dec 761 if vv < -242
tr inc 621 if zhz <= 59
ig dec 26 if y != 1581
txm dec 376 if y < 1581
low inc -726 if jaq > -1066
txm dec -152 if es >= -3865
wby inc 323 if jaq <= -1048
low dec 884 if zhz > 57
n dec 112 if t != 3062
ig inc 332 if xho != -1878
afo dec -295 if low > -4150
wby dec -158 if nm <= -47
low inc 824 if n <= -790
ipq dec -90 if y != 1570
j inc 41 if tq <= -608
txm inc 712 if j == -4613
n dec 557 if n < -793
ipq dec 340 if xho != -1885
wby dec 699 if wby <= -721
ipq dec 468 if y <= 1577
es inc 999 if ixp <= 3077
j dec -798 if es == -2858
es inc 652 if mux <= -4084
ntk inc -262 if ntk == -1589
n dec 528 if afo <= -758
vv dec 511 if es > -2861
ntk inc 78 if ixp < 3076
qen inc 502 if j < -3819
afo inc 363 if jaq < -1052
nm inc 215 if wby == -1428
s inc 608 if tq < -619
txm dec 100 if low >= -3324
mux inc 516 if mux > -4085
cwp inc -888 if ntk >= -1856
wby inc -941 if afo >= -408
tr dec 686 if cwp <= -1684
ig inc -365 if jaq > -1061
tq inc -593 if zhz == 64
cwp inc -867 if wby == -2369
ixp inc 736 if wby <= -2370
qen inc -240 if tq <= -1204
wby dec -961 if vv <= -759
txm inc 132 if ixp > 3075
ig inc 984 if n < -1877
ixp inc -898 if vv >= -769
wby inc -517 if es >= -2864
mux dec 735 if afo <= -396
n dec 971 if txm != 2290
t dec -329 if cwp > -2568
ig dec -780 if y < 1573
ixp dec 955 if tr <= -471
wby inc 545 if wby <= -1920
cwp dec -506 if afo == -400
afo inc 191 if vv > -764
afo inc -314 if s <= -13
ig dec -7 if tr > -486
afo dec 643 if n >= -1872
low dec -477 if nm != 183
t dec 98 if xho != -1885
nm dec -343 if zhz == 64
n dec -941 if zhz == 64
cwp inc 222 if ixp != 1218
zhz inc 756 if nm > 522
nm dec -233 if cwp != -2329
n inc 158 if es >= -2864
j dec 447 if ipq >= -1189
vv dec 236 if es <= -2852
txm dec -940 if j < -4264
low dec -631 if ntk == -1851
t inc 552 if tr != -479
nm inc -390 if vv >= -1006
ipq inc -589 if j <= -4263
tq dec -273 if n >= -785
wby dec 62 if jaq > -1062
t inc 897 if qen == 3639
ipq inc -541 if nm >= 355
jaq dec 169 if wby != -1452
mux dec 922 if nm > 354
y dec -185 if t < 3391
tq inc -170 if es > -2864
cwp inc -459 if es != -2849
tr dec -384 if ntk > -1854
vv inc 776 if s < -26
ixp dec 603 if vv >= -1005
ig dec -979 if low == -2213
qen inc -345 if jaq < -1216
s dec -931 if tr != -100
s dec 252 if ipq != -1721
wby inc 487 if tq <= -1096
tq inc 653 if ntk < -1841
xho dec -526 if y == 1756
ipq inc -563 if txm >= 2283
nm inc -679 if qen == 3287
cwp dec 223 if y > 1754
es inc 852 if tq < -447
es dec 455 if txm >= 2285
ntk dec 787 if y != 1749
tr inc 873 if xho < -1352
ig inc 690 if txm < 2281
cwp inc -367 if y != 1754
ig dec 502 if j < -4263
j dec -144 if cwp <= -3379
zhz inc 287 if mux >= -5222
ntk dec -34 if jaq != -1231
y inc -592 if ntk != -2610
afo inc 296 if s != 667
ixp inc 614 if es > -2465
jaq dec 424 if t <= 3385
ipq inc 894 if ntk > -2609
y dec 484 if afo == -230
jaq inc -458 if zhz <= 354
n dec -547 if ntk == -2604
t inc -683 if jaq <= -1689
t dec -703 if afo > -233
afo inc -998 if nm != -314
vv inc 695 if jaq < -1677
afo inc 986 if tq == -450
es dec -29 if ntk != -2612
ixp inc -666 if zhz > 359
txm inc -505 if low != -2213
qen inc -980 if txm > 2285
tq dec 900 if vv == -303
tr inc -728 if nm > -325
low dec -702 if ipq < -1391
mux inc -9 if ixp >= 1236
j dec -777 if s == 665
ig inc 527 if ipq < -1388
es inc 251 if n != -242
nm inc -979 if txm == 2290
wby inc -524 if tq == -1350
zhz dec -886 if txm < 2294
nm inc -296 if zhz > 1232
j inc 114 if wby >= -1478
cwp dec -142 if s <= 667
n inc -286 if es != -2180
jaq inc 681 if nm > -1591
mux inc -299 if txm > 2289
vv dec 602 if es == -2181
nm inc -118 if s == 653
s dec 176 if j != -4111
low dec -469 if tr >= 50
xho dec 775 if txm == 2290
mux inc -164 if ipq < -1391
tq dec 695 if tr < 48
afo inc 93 if n < -515
cwp inc -973 if nm > -1596
vv dec 517 if es < -2179
low dec -353 if wby == -1479
ixp dec -742 if ixp == 1235
tr inc -76 if vv < -1421
ixp inc -26 if txm != 2281
qen inc -205 if tq != -1347
zhz inc -178 if zhz > 1232
xho dec -540 if qen < 2109
nm inc -650 if j > -4123
es inc -191 if xho == -1594
txm dec -939 if wby == -1479
ig inc 658 if wby > -1471
xho dec -234 if n <= -529
nm dec -510 if ixp == 1958
ig inc 856 if low >= -682
ipq dec -415 if ixp <= 1959
wby inc -471 if vv > -1417
t inc -104 if zhz < 1062
low dec -345 if tq <= -1348
xho inc -163 if wby <= -1485
low inc 594 if ipq < -980
ipq dec 754 if ixp != 1951
ixp inc 263 if y != 680
vv inc -186 if mux != -5685
vv inc 436 if t < 3993
cwp dec 276 if xho >= -1595
t dec -268 if t >= 3983
j dec -241 if cwp == -4494
wby dec -883 if ipq < -972
afo inc 881 if ntk == -2604
ntk inc -543 if ntk < -2602
mux dec 524 if mux <= -5688
ixp inc -15 if low != 243
jaq dec -21 if wby < -590
tq dec 728 if jaq == -1663
vv dec -831 if low >= 241
j dec 290 if vv > -148
ig dec -192 if cwp == -4494
zhz dec 684 if j < -3874
tq dec 807 if cwp <= -4489
t inc -979 if y < 689
es inc 853 if qen >= 2095
s dec 211 if ipq >= -982
t inc -113 if j != -3880
tr inc -801 if wby >= -598
afo inc -405 if es < -1511
ipq inc -980 if y == 680
qen inc -345 if n >= -520
n dec 332 if n < -515
mux dec -593 if zhz <= 380
vv dec -97 if j > -3881
mux inc 321 if y >= 679
mux inc 299 if n > -857
tr dec 105 if qen <= 1757
y dec 746 if jaq != -1673
qen inc 898 if afo <= 333
s inc 467 if afo < 330
xho dec -634 if txm <= 3233
ig inc -288 if es <= -1512
es dec -371 if xho > -970
xho inc -421 if txm < 3235
ntk dec -803 if tr == -939
xho dec -762 if es != -1150
ixp dec -336 if afo > 325
es dec 88 if ixp >= 2273
n dec -586 if afo >= 321
zhz inc -260 if tq == -2885
qen dec 94 if tr < -930
mux inc -37 if j <= -3868
es inc 438 if s <= 733
mux dec 946 if mux < -4501
qen inc -341 if s > 746
cwp inc -773 if cwp != -4485
txm dec 836 if vv == -58
y inc 495 if wby > -603
txm inc -286 if ixp > 2270
jaq inc 147 if txm > 2104
tq dec -65 if j > -3878
cwp dec 733 if n >= -272
n inc -65 if j >= -3884
nm inc 582 if qen != 2563
y dec 719 if j >= -3877
vv inc 636 if vv <= -51
es dec -295 if y > -299
n dec 288 if cwp > -6006
ntk inc -40 if afo >= 322
tr inc -197 if s != 732
j inc -224 if xho < -626
mux dec 465 if zhz > 123
s dec 633 if ixp <= 2275
ipq inc 307 if tq == -2812
s dec 336 if s > 96
low inc 4 if vv == 578
t inc -665 if afo != 327
qen inc -403 if t >= 3161
t dec 586 if t >= 3160
txm dec 745 if tq <= -2816
mux dec -443 if nm != -1654
vv dec 370 if y < -297
vv dec -198 if vv == 578
n dec -470 if s > -240
cwp dec 166 if wby >= -598
xho dec 478 if cwp != -6171
n dec -914 if zhz >= 110
txm inc 494 if ixp >= 2264
tq inc -742 if n < 774
tr dec -748 if wby >= -599
wby inc 421 if es == -853
ipq inc -900 if qen < 2166
s dec 452 if es != -853
afo dec -929 if ntk >= -3178
qen inc 706 if jaq == -1516
ipq dec 58 if j != -3868
afo inc -331 if s < -221
ntk dec -186 if ipq == -2919
jaq dec -371 if y < -289
cwp dec -975 if n == 766
qen inc -631 if xho != -1090
cwp dec 57 if ig < 1779
ixp inc -146 if ipq <= -2922
s inc -148 if y < -283
zhz dec 842 if mux < -5009
nm inc -840 if ipq != -2923
mux inc -583 if wby < -171
j dec -467 if ig >= 1788
cwp inc -513 if j == -3877
s inc 353 if ntk != -3001
s dec 731 if ixp > 2273
tr inc 928 if afo != 3
vv dec 947 if jaq != -1140
tq inc 184 if ig <= 1785
vv dec 571 if s != -375
n dec 222 if vv == -742
ig dec 812 if qen <= 2235
s inc 722 if y > -294
afo dec 278 if cwp != -5708
tr dec 897 if low < 245
es inc 439 if afo == -282
zhz inc 976 if vv != -750
low dec 381 if ipq != -2927
zhz dec -510 if zhz >= 242
mux dec -846 if tr <= 555
tq dec 786 if y <= -293
wby inc 40 if wby >= -180
wby inc 461 if wby != -131
y inc -388 if t < 2579
ig dec 40 if ntk < -2992
txm inc -177 if nm > -2503
j inc -922 if tq == -3378
tr dec 739 if ipq != -2917
y dec 776 if mux == -4749
s inc 261 if afo != -273
s dec 28 if y != -1463
es dec 156 if txm > 1679
low inc -914 if es == -413
xho inc 923 if s <= 584
txm inc -979 if txm >= 1671
jaq inc -102 if nm >= -2503
jaq dec -802 if low >= -122
mux inc 183 if low > -129
zhz dec -428 if nm == -2500
tq inc -712 if txm >= 692
tq dec -624 if txm < 705
zhz inc -472 if tr <= -199
tr dec 568 if nm < -2495
tr dec 602 if ixp > 2272
tr inc -283 if zhz == 1187
ntk inc 387 if ixp < 2277
tr inc 11 if wby <= 331
es dec -830 if txm != 692
nm inc -957 if qen > 2233
xho inc 564 if tq > -3475
s dec 145 if n < 545
nm inc -366 if cwp == -5704
t dec 752 if afo > -283
ig inc 881 if ipq >= -2911
zhz inc -139 if cwp != -5708
afo inc 926 if wby > 317
jaq dec 494 if es >= 412
j inc 92 if j <= -4792
txm inc -93 if jaq > -1743
tq dec 776 if y <= -1447
jaq inc 696 if zhz == 1048
mux dec -327 if j >= -4713
zhz dec 432 if vv >= -742
tq inc 100 if t > 1823
t inc 684 if qen <= 2238
txm dec 251 if mux > -4243
low dec -387 if j != -4702
txm inc -736 if qen <= 2239
mux dec -658 if qen < 2237
n inc -314 if low < 268
t dec -246 if mux < -3575
xho dec -143 if cwp > -5712
low inc 83 if t <= 2755
ixp dec 503 if y > -1460
cwp dec 18 if qen <= 2235
y inc 390 if zhz != 616
ntk dec -289 if ipq > -2927
s inc -982 if low <= 343
jaq dec -701 if ntk <= -2327
n inc -62 if j >= -4703
nm inc 870 if tr >= -1037
xho inc -962 if j > -4715
afo inc 226 if y != -1459
j dec -510 if s != -557
low inc -1 if afo != 879
jaq inc -847 if qen > 2231
jaq dec 360 if t <= 2760
j inc -770 if qen > 2228
low dec -965 if y < -1452
j inc 553 if mux == -3585
mux dec 300 if t <= 2756
xho inc 495 if tq > -4150
cwp inc -730 if ipq < -2912
tq inc -315 if n >= 235
j inc -995 if mux >= -3877
y inc 378 if txm < -377
vv dec -898 if t >= 2749
nm inc -432 if jaq > -2249
ipq inc 850 if qen > 2230
afo inc 853 if s != -558
vv dec 628 if low != 1314
y inc -182 if ntk <= -2323
wby inc 270 if j < -4958
n dec 685 if j < -4976
low inc -33 if s != -548
wby dec -502 if jaq < -2243
nm inc -400 if xho < 69
jaq dec -404 if t > 2744
mux dec 668 if t > 2748
wby inc -308 if xho > 63
low dec 790 if ixp > 1766
ixp inc -581 if vv > -481
es dec -399 if y <= -1258
ixp inc -839 if tq >= -4146
jaq inc 451 if low == 479
mux dec 733 if j < -4957
cwp inc 263 if xho == 66
y dec -522 if ntk < -2319
t dec 764 if ixp <= 356
vv inc 458 if es != 819
afo dec 798 if xho < 67
wby dec -902 if zhz > 616
ixp inc 881 if s > -554
ntk inc -955 if s != -550
nm inc -890 if n >= 229
n dec -214 if j > -4972
wby inc -553 if txm <= -388
s dec -319 if tr != -1033
t inc 18 if mux < -5272
tr dec 923 if zhz <= 625
xho inc -917 if afo != 927
tq dec -518 if tr <= -1950
j inc -895 if ipq != -2066
low inc 23 if y > -738
vv inc 31 if zhz >= 611
cwp inc 447 if y >= -742
nm inc 702 if cwp <= -5746
ig dec -733 if low <= 512
es inc -577 if vv <= 24
y inc 697 if nm < -3280
s inc -434 if y != -31
tq dec 694 if ipq == -2069
t inc -759 if n > 442
jaq inc 223 if s >= -670
zhz inc 811 if txm < -374
mux inc 25 if nm > -3277
jaq dec 825 if nm < -3283
tr dec -412 if n >= 438
txm inc 341 if mux >= -5283
j dec -952 if zhz == 1427
qen inc 392 if cwp <= -5752
zhz inc 486 if txm == -39
afo inc -500 if wby == 790
s inc -208 if zhz >= 1912
afo dec -937 if xho != -849
ig inc -618 if y > -41
ntk inc -11 if qen > 2229
vv inc 34 if s <= -873
ixp inc 535 if cwp == -5742
ipq dec -996 if mux != -5290
s dec 436 if qen != 2224
ntk inc 250 if wby != 790
low dec -959 if txm > -46
y inc -807 if es <= 244
es inc -109 if txm == -39
y dec -720 if y != -846
t inc -658 if jaq < -2445
ntk inc -522 if y == -846
zhz inc -911 if low <= 1475
zhz inc 313 if vv <= 59
afo inc -631 if zhz <= 1319
low inc -18 if t == 595
ipq inc 16 if xho >= -855
cwp inc 841 if ig > 1034
es dec -31 if wby <= 781
ipq dec 904 if mux != -5286
cwp inc 309 if ntk > -2859
txm dec 413 if jaq != -2442
mux dec 237 if ixp <= 1771
vv inc -891 if ipq >= -1969
s dec -4 if n != 444
cwp dec 971 if s == -1309
mux inc -388 if es < 122
es inc -687 if txm < -442
ixp inc 95 if t != 591
jaq dec -333 if y >= -851
ixp dec -134 if j >= -4910
mux inc 496 if zhz > 1313
j dec -568 if afo >= 722
ig dec -234 if wby >= 781
mux dec 814 if xho > -856
jaq inc 78 if nm != -3276
nm dec -942 if ig < 1287
vv inc 397 if zhz == 1315
n inc 86 if wby != 790
ig dec -202 if s < -1306
j dec 0 if afo >= 728
ntk dec -320 if vv > -438
tr dec -962 if ipq <= -1957
es inc -424 if xho >= -843
txm dec 972 if ig <= 1487
cwp dec -730 if vv < -436
ipq dec -691 if ig == 1480
t inc -852 if afo == 731
ntk inc -361 if es <= -563
low inc -413 if n > 436
afo dec -818 if ixp != 1892
cwp dec -16 if wby >= 791
s inc 884 if ntk <= -2853
ipq inc -523 if low >= 1061
mux inc -157 if tq < -4309
low inc -917 if low < 1054
low dec -679 if zhz > 1309
zhz dec -426 if ntk <= -2866
tr dec 995 if y < -838
low inc -343 if jaq != -2039
wby dec 906 if ixp != 1899
txm inc 805 if n == 444
jaq inc -628 if nm <= -2343
t inc 464 if es != -554
qen dec 133 if low >= 821
xho dec -729 if zhz > 1319
xho inc -186 if zhz <= 1315
zhz dec 798 if s <= -417
jaq inc -726 if zhz <= 521
ig dec -214 if zhz > 513
low inc 486 if es == -558
ig dec -328 if y != -851
tq inc -112 if ixp != 1905
ipq inc 53 if afo != 1546
ntk inc 694 if es != -567
jaq dec -394 if tq <= -4427
ixp dec -575 if s <= -423
ig dec 694 if tr >= -1579
mux dec -573 if nm > -2345
vv inc -966 if tr <= -1576
vv inc 135 if tq >= -4435
afo dec -140 if jaq >= -3007
jaq inc 543 if ipq != -1217
vv inc -145 if vv == -1274
wby inc -788 if cwp < -4825
jaq inc -180 if zhz <= 518
vv inc -248 if vv >= -1425
zhz inc -174 if xho == -1037
t dec -570 if t <= 207
afo inc -422 if cwp > -4838
wby inc -494 if low >= 1297
s inc 20 if vv <= -1661
s dec -349 if ig > 1323
s inc -399 if txm > -627
tr inc 59 if ixp == 2474
t inc -490 if wby > -499
jaq dec 265 if ixp <= 2480
afo inc 77 if qen > 2234
ntk inc -683 if ixp != 2481
tr dec -127 if afo > 1262
low inc -239 if jaq < -3438
j inc 840 if ipq >= -1220
n dec 197 if vv > -1677
low dec 896 if nm > -2352
wby inc 26 if vv > -1663
txm inc 552 if low > 162
jaq inc -948 if nm >= -2347
tr dec -871 if txm == -67
s dec -498 if n >= 246
n inc -934 if mux >= -5430
j inc -113 if ntk < -2849
tq dec -636 if cwp < -4828
j inc 114 if cwp >= -4832
tq dec 557 if txm != -71
ipq inc 392 if ig <= 1335
low dec 739 if tq != -4343
s dec -853 if t < 288
n dec 715 if txm >= -75
jaq inc 140 if ixp == 2474
n dec 896 if xho < -1030
ipq inc -359 if xho < -1031
ixp dec -497 if j != -3506
afo inc -293 if low < -566
jaq inc 89 if nm > -2345
ntk dec -524 if vv >= -1669
ntk dec 371 if s < 897
s dec -478 if jaq > -4170
j inc 924 if ipq == -1184
ntk inc -239 if y == -846
wby inc -397 if ntk >= -2941
nm dec 907 if qen == 2233"""
