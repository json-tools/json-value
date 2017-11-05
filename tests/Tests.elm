module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import JsonValue
    exposing
        ( JsonValue(ObjectValue, ArrayValue, BoolValue, StringValue, NumericValue, NullValue)
        , setIn
        , getIn
        , setPropertyName
        )
import Json.Encode as Encode exposing (object, list, string, int, float, bool, null)
import Json.Decode as Decode


--import Fuzz exposing (Fuzzer, list, int, string)


suite : Test
suite =
    describe "JsonValue"
        [ test "decoder" <|
            \() ->
                [ ( "str", string "value" )
                , ( "array", list [ int 10, float 0.1, bool False, null ] )
                ]
                    |> object
                    |> Decode.decodeValue JsonValue.decoder
                    |> Expect.equal
                        (Ok <|
                            ObjectValue
                                [ ( "str", StringValue "value" )
                                , ( "array", ArrayValue [ NumericValue 10, NumericValue 0.1, BoolValue False, NullValue ] )
                                ]
                        )
        , test "encoder" <|
            \() ->
                [ ( "str", StringValue "value" )
                , ( "array", ArrayValue [ NumericValue 10, NumericValue 0.1, BoolValue False, NullValue ] )
                ]
                    |> ObjectValue
                    |> JsonValue.encode
                    |> Expect.equal
                        ([ ( "str", string "value" )
                         , ( "array", list [ int 10, float 0.1, bool False, null ] )
                         ]
                            |> object
                        )
        , describe "getIn"
            [ test "simple object" <|
                \() ->
                    ObjectValue [ ( "foo", StringValue "bar" ) ]
                        |> getIn [ "foo" ]
                        |> Expect.equal (Ok <| StringValue "bar")
            ]
        , describe "setIn"
            [ test "object" <|
                \() ->
                    NullValue
                        |> inObjWithProp "foo"
                        |> setIn [ "foo" ] (StringValue "bar")
                        |> Expect.equal
                            (StringValue "bar"
                                |> inObjWithProp "foo"
                                |> Ok
                            )
            , test "slightly nested object" <|
                \() ->
                    NullValue
                        |> inObjWithProp "bar"
                        |> inObjWithProp "foo"
                        |> setIn [ "foo", "bar" ] (StringValue "baz")
                        |> Expect.equal
                            (StringValue "baz"
                                |> inObjWithProp "bar"
                                |> inObjWithProp "foo"
                                |> Ok
                            )
            , test "slightly more nested object" <|
                \() ->
                    NullValue
                        |> inObjWithProp "bar"
                        |> inObjWithProp "foo"
                        |> inObjWithProp "root"
                        |> setIn [ "root", "foo", "bar" ] (StringValue "baz")
                        |> Expect.equal
                            (StringValue "baz"
                                |> inObjWithProp "bar"
                                |> inObjWithProp "foo"
                                |> inObjWithProp "root"
                                |> Ok
                            )
            , test "deeply nested object" <|
                \() ->
                    NullValue
                        |> inObjWithProp "bar"
                        |> inObjWithProp "foo"
                        |> inObjWithProp "root"
                        |> inObjWithProp "rootest"
                        |> setIn [ "rootest", "root", "foo", "bar" ] (StringValue "baz")
                        |> Expect.equal
                            (StringValue "baz"
                                |> inObjWithProp "bar"
                                |> inObjWithProp "foo"
                                |> inObjWithProp "root"
                                |> inObjWithProp "rootest"
                                |> Ok
                            )
            , test "quite deeply nested object" <|
                \() ->
                    NullValue
                        |> inObjWithProp "bar"
                        |> inObjWithProp "foo"
                        |> inObjWithProp "root"
                        |> inObjWithProp "rootest"
                        |> inObjWithProp "_"
                        |> setIn [ "_", "rootest", "root", "foo", "bar" ] (StringValue "baz")
                        |> Expect.equal
                            (StringValue "baz"
                                |> inObjWithProp "bar"
                                |> inObjWithProp "foo"
                                |> inObjWithProp "root"
                                |> inObjWithProp "rootest"
                                |> inObjWithProp "_"
                                |> Ok
                            )
            , test "array" <|
                \() ->
                    NullValue
                        |> inArray
                        |> setIn [ "0" ] (StringValue "bar")
                        |> Expect.equal
                            (StringValue "bar"
                                |> inArray
                                |> Ok
                            )
            , test "array of arrays" <|
                \() ->
                    NullValue
                        |> inArray
                        |> inArray
                        |> setIn [ "0", "0" ] (StringValue "bar")
                        |> Expect.equal
                            (StringValue "bar"
                                |> inArray
                                |> inArray
                                |> Ok
                            )
            , test "array of arrays of arrays" <|
                \() ->
                    NullValue
                        |> inArray
                        |> inArray
                        |> inArray
                        |> setIn [ "0", "0", "0" ] (StringValue "bar")
                        |> Expect.equal
                            (StringValue "bar"
                                |> inArray
                                |> inArray
                                |> inArray
                                |> Ok
                            )
            , test "array of object" <|
                \() ->
                    NullValue
                        |> inObjWithProp "foo"
                        |> inArray
                        |> setIn [ "0", "foo" ] (StringValue "bar")
                        |> Expect.equal
                            (StringValue "bar"
                                |> inObjWithProp "foo"
                                |> inArray
                                |> Ok
                            )
            , test "object with siblings" <|
                \() ->
                    NullValue
                        |> inObjWithProp "foo"
                        |> setIn [ "bar" ] (StringValue "bar")
                        |> Expect.equal
                            (Ok (ObjectValue ([ ( "foo", NullValue ), ( "bar", StringValue "bar" ) ])))
            ]
        , describe "setPropertyName"
            [ test "simple object" <|
                \() ->
                    StringValue "bar"
                        |> inObjWithProp "foo"
                        |> setPropertyName ( [], 0 ) "bam"
                        |> Expect.equal (Ok <| ObjectValue <| [ ( "bam", StringValue "bar" ) ])
            ]
        ]


inObjWithProp : String -> JsonValue -> JsonValue
inObjWithProp name p =
    ObjectValue [ ( name, p ) ]


inArray : JsonValue -> JsonValue
inArray p =
    ArrayValue [ p ]
