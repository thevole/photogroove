port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (id, checked, class, classList, type_, title, src, max, name)
import Html.Events exposing (on, onClick)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (Value, float, string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    , status : String
    }


port setFilters : FilterOptions -> Cmd msg


port statusChanges : (Value -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SetStatus String
    | SetError String


type ThumbnailSize
    = Small
    | Medium
    | Large


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , canvas [ id "main-canvas", class "large" ] []
        ]


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attrs.max "11", onImmediateValueChanged toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]")
        , classList
            [ ( "selected"
              , selectedUrl == Just thumbnail.url
              )
            ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : Model -> ThumbnailSize -> Html Msg
viewSizeChooser model size =
    let
        isChecked : Bool
        isChecked =
            if size == model.chosenSize then
                True
            else
                False
    in
        label []
            [ input [ type_ "radio", checked isChecked, name "size", onClick (SetSize size) ] []
            , text (sizeToString size)
            ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    , status = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetStatus status ->
            ( { model | status = status }
            , Cmd.none
            )

        SetError error ->
            ( { model | status = error }
            , Cmd.none
            )

        SelectByUrl selectedUrl ->
            applyFilters { model | selectedUrl = Just selectedUrl }

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                applyFilters { model | selectedUrl = newSelectedUrl }

        SurpriseMe ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model
                , Random.generate SelectByIndex randomPhotoPicker
                )

        SetSize size ->
            ( { model | chosenSize = size }
            , Cmd.none
            )

        LoadPhotos (Ok photos) ->
            applyFilters
                { model
                    | photos = photos
                    , selectedUrl = Maybe.map .url (List.head photos)
                }

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error! (Try turning it off and on again.)"
              }
            , Cmd.none
            )

        SetHue hue ->
            applyFilters { model | hue = hue }

        SetRipple ripple ->
            applyFilters { model | ripple = ripple }

        SetNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )

        Nothing ->
            ( model, Cmd.none )


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        status : String
        status =
            case Json.Decode.decodeValue float flags of
                Ok version ->
                    "Initializing Pasta v" ++ toString version

                Err _ ->
                    "Initialization error"
    in
        ( { initialModel | status = status }
        , initialCmd
        )


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = subscriptions
        }


processStatus : Value -> Msg
processStatus value =
    case Json.Decode.decodeValue string value of
        Ok status ->
            SetStatus status

        Err error ->
            SetError error


subscriptions : Model -> Sub Msg
subscriptions _ =
    statusChanges processStatus


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


onImmediateValueChanged : (Int -> msg) -> Attribute msg
onImmediateValueChanged toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"
