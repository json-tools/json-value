module Main exposing (main)

import Html exposing (Html, program)
import Html.Attributes exposing (style, rows, cols)
import Html.Events exposing (onInput)
import Json.Encode as Encode exposing (object, list, int, float, bool, string, null)
import Json.Decode as Decode exposing (Value, decodeValue)
import JsonValue exposing (JsonValue(ObjectValue, ArrayValue, StringValue, NumericValue, BoolValue))


type alias Model =
    { object : Value
    , jsonString : String
    }


type Msg
    = JsonChanged String


main : Program Never Model Msg
main =
    program { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }


init : ( Model, Cmd Msg )
init =
    Model objectToInspect (Encode.encode 4 objectToInspect) ! []


objectToInspect : Value
objectToInspect =
    [ ( "foo", string "baz" )
    , ( "bar", int 1 )
    , ( "array", list [ int 2, bool True, int 4 ] )
    ]
        |> object


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonChanged json ->
            { model
                | jsonString = json
                , object =
                    json
                        |> Decode.decodeString Decode.value
                        |> Result.withDefault model.object
            }
                ! []


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.textarea
            [ onInput JsonChanged
            , rows 20
            , cols 80
            , style
                [ ( "font-family", "menlo, monospace" )
                , ( "padding", "10px" )
                ]
            ]
            [ Html.text model.jsonString ]
        , inspect model.object
        ]


inspect : Value -> Html Msg
inspect value =
    case decodeValue JsonValue.decoder value of
        Ok jv ->
            Html.div [ style [ ( "font-family", "menlo, monospace" ), ( "font-size", "12px" ) ] ] [ print jv ]

        Err str ->
            Html.text str


print : JsonValue -> Html Msg
print jv =
    case jv of
        ObjectValue obj ->
            obj
                |> List.map (\( k, v ) -> Html.div [] [ Html.strong [] [ Html.text k ], Html.text ": ", print v ])
                |> Html.div [ style [ ( "padding-left", "2ch" ) ] ]

        ArrayValue list ->
            list
                |> List.indexedMap (\index v -> Html.div [] [ Html.strong [] [ Html.text <| toString <| index ], Html.text ": ", print v ])
                |> Html.div [ style [ ( "padding-left", "2ch" ) ] ]

        NumericValue v ->
            toString v
                |> Html.text

        BoolValue b ->
            if b then
                "true" |> Html.text
            else
                "false" |> Html.text

        StringValue s ->
            Html.text s

        s ->
            toString s
                |> Html.text
