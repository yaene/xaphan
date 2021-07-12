module Main exposing (main)

import Browser
import Dict exposing (update)
import Html


type alias Model =
    String


type Msg
    = Noop


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( "Hello World", Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
