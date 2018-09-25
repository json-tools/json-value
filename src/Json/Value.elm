module Json.Value exposing
    ( JsonValue(..)
    , getIn
    , setIn, setPropertyName, deleteIn
    , decoder, encode
    , decodeValue
    )

{-|


# Definitions

@docs JsonValue


# Reading

@docs getIn


# Manipulation

@docs setIn, setPropertyName, deleteIn


# Transforms

@docs decoder, encode


# Helpers

@docs decodeValue

-}

import Json.Decode as Decode exposing (Decoder, Value, succeed)
import Json.Encode as Encode


{-| Type representing json value according to spec
-}
type JsonValue
    = ObjectValue (List ( String, JsonValue ))
    | ArrayValue (List JsonValue)
    | BoolValue Bool
    | NullValue
    | NumericValue Float
    | StringValue String


{-| Decoder for JsonValue

    [ ( "str", Json.Encode.string "value" )
    , ( "array"
      , Json.Encode.list
            [ Json.Encode.int 10
            , Json.Encode.float 0.1
            , Json.Encode.bool False
            , Json.Encode.null
            ]
      )
    ]
        |> object
        |> Json.Decode.decodeValue decoder
        |> Expect.equal
            (Ok <|
                ObjectValue
                    [ ( "str", StringValue "value" )
                    , ( "array"
                      , ArrayValue
                            [ NumericValue 10
                            , NumericValue 0.1
                            , BoolValue False
                            , NullValue
                            ]
                      )
                    ]
            )

-}
decoder : Decoder JsonValue
decoder =
    let
        objectValueDecoder =
            Decode.keyValuePairs (Decode.lazy (\_ -> decoder))
                --|> Decode.andThen (List.reverse >> succeed)
                |> Decode.map ObjectValue

        arrayValueDecoder =
            Decode.list (Decode.lazy (\_ -> decoder))
                |> Decode.map ArrayValue
    in
    Decode.oneOf
        [ objectValueDecoder
        , arrayValueDecoder
        , Decode.null NullValue
        , Decode.string |> Decode.map StringValue
        , Decode.float |> Decode.map NumericValue
        , Decode.bool |> Decode.map BoolValue
        ]


{-| Encoder for JsonValue

    [ ( "str", StringValue "value" )
    , ( "array"
      , ArrayValue
            [ NumericValue 10
            , NumericValue 0.1
            , BoolValue False
            , NullValue
            ]
      )
    ]
        |> ObjectValue
        |> encode
        |> Expect.equal
            ([ ( "str", Json.Encode.string "value" )
             , ( "array"
               , Json.Encode.list
                    [ Json.Encode.int 10
                    , Json.Encode.float 0.1
                    , Json.Encode.bool False
                    , Json.Encode.null
                    ]
               )
             ]
                |> object
            )

-}
encode : JsonValue -> Value
encode v =
    case v of
        ObjectValue ov ->
            ov
                |> List.map (\( key, jv ) -> ( key, encode jv ))
                |> Encode.object

        ArrayValue av ->
            av
                |> Encode.list encode

        StringValue s ->
            Encode.string s

        BoolValue b ->
            Encode.bool b

        NullValue ->
            Encode.null

        NumericValue n ->
            Encode.float n


{-| Rename property in json value

    StringValue "bar"
        |> inObjWithProp "foo"
        |> setPropertyName ( [], 0 ) "bam"
        |> Expect.equal (Ok <| ObjectValue <| [ ( "bam", StringValue "bar" ) ])

-}
setPropertyName : ( List String, Int ) -> String -> JsonValue -> Result String JsonValue
setPropertyName ( pathInJson, index ) newName hostValue =
    let
        renameKey val =
            case val of
                ObjectValue v ->
                    v
                        |> List.indexedMap
                            (\i ( k, value ) ->
                                ( if index == i then
                                    newName

                                  else
                                    k
                                , value
                                )
                            )
                        |> ObjectValue
                        >> Ok

                _ ->
                    Err "Can not rename property of this json value"

        targetValue =
            hostValue
                |> getIn pathInJson
                |> Result.andThen renameKey
                |> Result.withDefault hostValue
    in
    setIn pathInJson targetValue hostValue


{-| Set json value at given path

    ObjectValue [ ( "foo", NullValue ) ]
        |> setIn [ "foo" ] (StringValue "bar")
        |> Expect.equal
            (Ok (ObjectValue [ ( "foo", StringValue "bar" ) ]))

-}
setIn : List String -> JsonValue -> JsonValue -> Result String JsonValue
setIn pathInJson valueToSet hostValue =
    let
        path =
            pathInJson
                |> List.reverse

        newValue =
            case path of
                [] ->
                    Ok valueToSet

                _ :: subpath ->
                    path
                        |> List.foldl
                            (\key ( thisPath, value ) ->
                                let
                                    p =
                                        List.reverse thisPath

                                    v =
                                        value
                                            |> Result.map
                                                (\vv ->
                                                    hostValue
                                                        |> getIn p
                                                        |> Result.andThen (setProperty key vv)
                                                        |> Result.withDefault (ObjectValue [ ( key, vv ) ])
                                                )
                                in
                                case thisPath of
                                    [] ->
                                        ( [], v )

                                    _ :: tail ->
                                        ( tail, v )
                            )
                            ( subpath, Ok valueToSet )
                        |> (\( _, v ) -> v)
    in
    newValue


{-| Get json value at given path

    ObjectValue [ ( "foo", StringValue "bar" ) ]
        |> getIn [ "foo" ]
        |> Expect.equal (Ok <| StringValue "bar")

-}
getIn : List String -> JsonValue -> Result String JsonValue
getIn path value =
    case path of
        [] ->
            Ok value

        head :: tail ->
            case value of
                ObjectValue v ->
                    v
                        |> List.foldl
                            (\( key, val ) res ->
                                if res /= Nothing then
                                    res

                                else if key == head then
                                    Just val

                                else
                                    Nothing
                            )
                            Nothing
                        |> Result.fromMaybe "Key not found"
                        |> Result.andThen (getIn tail)

                ArrayValue v ->
                    head
                        |> String.toInt
                        |> Result.fromMaybe "Not an integer"
                        |> Result.andThen
                            (\index ->
                                v
                                    |> List.drop index
                                    |> List.head
                                    |> Result.fromMaybe "Index is too big"
                            )
                        |> Result.andThen (getIn tail)

                _ ->
                    Err "You are trying to access property of something that is not object or array"


setProperty : String -> JsonValue -> JsonValue -> Result String JsonValue
setProperty key value object =
    let
        updateOrAppend list =
            if List.any (\( k, _ ) -> k == key) list then
                list
                    |> List.map
                        (\( k, v ) ->
                            if k == key then
                                ( key, value )

                            else
                                ( k, v )
                        )

            else
                list ++ [ ( key, value ) ]
    in
    case object of
        ObjectValue o ->
            o
                |> updateOrAppend
                |> ObjectValue
                |> Ok

        ArrayValue list ->
            let
                index =
                    key
                        |> Decode.decodeString Decode.int
                        |> Result.withDefault (List.length list)
            in
            if List.length list > index then
                list
                    |> List.indexedMap
                        (\i v ->
                            if i == index then
                                value

                            else
                                v
                        )
                    |> ArrayValue
                    |> Ok

            else
                list
                    ++ [ value ]
                    |> ArrayValue
                    |> Ok

        _ ->
            if key == "0" then
                [ value ]
                    |> ArrayValue
                    |> Ok

            else
                [ ( key, value ) ]
                    |> ObjectValue
                    |> Ok


{-|

    Delete path in json object

-}
deleteIn : List String -> JsonValue -> Result String JsonValue
deleteIn pathInJson hostValue =
    let
        ( key, path ) =
            pathInJson
                |> List.reverse
                |> (\x ->
                        case x of
                            k :: revPath ->
                                ( Just k, List.reverse revPath )

                            [] ->
                                ( Nothing, [] )
                   )

        rejectKey keyToReject val =
            case val of
                ObjectValue res ->
                    res
                        |> List.filter (\( k, _ ) -> k /= keyToReject)
                        |> (ObjectValue >> Ok)

                ArrayValue res ->
                    res
                        |> List.indexedMap (\ind v -> ( ind |> String.fromInt, v ))
                        |> List.filter (\( k, _ ) -> k /= keyToReject)
                        |> List.map (\( _, v ) -> v)
                        |> (ArrayValue >> Ok)

                _ ->
                    Err "It is not possible to delete key when host value is not object or array"

        targetValue =
            case key of
                Just k ->
                    hostValue
                        |> getIn path
                        |> Result.andThen (rejectKey k)
                        |> Result.withDefault hostValue

                Nothing ->
                    hostValue
    in
    hostValue
        |> setIn path targetValue


{-| A helper function to decode value. JsonValue decoder always succeeds, this helper aims
to reduce unnecessary noise in code.
-}
decodeValue : Value -> JsonValue
decodeValue v =
    v
        |> Decode.decodeValue decoder
        |> Result.withDefault NullValue
