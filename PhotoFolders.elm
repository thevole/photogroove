module PhotoFolders exposing (main)

import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)
import Dict exposing (Dict)


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root =
        Folder
            { name = "Loading..."
            , photoUrls = []
            , subfolders = []
            , expanded = False
            }
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , modelDecoder
        |> Http.get "http://elm-in-action.com/folders/list"
        |> Http.send LoadPage
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi", { title = "Trevi", relatedUrls = [ "coli", "fresco" ], size = 34, url = "trevi" } )
                , ( "fresco", { title = "Fresco", relatedUrls = [ "trevi" ], size = 46, url = "fresco" } )
                , ( "coli", { title = "Coliseum", relatedUrls = [ "trevi", "fresco" ], size = 36, url = "coli" } )
                ]
        , root =
            Folder
                { name = "Photos"
                , expanded = True
                , photoUrls = []
                , subfolders =
                    [ Folder
                        { name = "2016"
                        , expanded = True
                        , photoUrls = [ "trevi", "coli" ]
                        , subfolders =
                            [ Folder { name = "outdoors", expanded = True, photoUrls = [], subfolders = [] }
                            , Folder
                                { name = "indoors", expanded = True, photoUrls = [ "fresco" ], subfolders = [] }
                            ]
                        }
                    , Folder
                        { name = "2017"
                        , expanded = True
                        , photoUrls = []
                        , subfolders =
                            [ Folder { name = "outdoors", expanded = True, photoUrls = [], subfolders = [] }
                            , Folder
                                { name = "indoors", expanded = True, photoUrls = [], subfolders = [] }
                            ]
                        }
                    ]
                }
        }


type Msg
    = SelectPhotoUrl String
    | LoadPage (Result Http.Error Model)
    | ToggleExpanded FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleExpanded path ->
            ( { model | root = toggleExpanded path model.root }
            , Cmd.none
            )

        SelectPhotoUrl url ->
            ( { model
                | selectedPhotoUrl = Just url
              }
            , Cmd.none
            )

        LoadPage (Ok newModel) ->
            ( newModel, Cmd.none )

        LoadPage (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
        div [ class "content" ]
            [ div [ class "folders" ]
                [ h1 [] [ text "Folders" ]
                , viewFolder End model.root
                ]
            , div [ class "selected-photo" ] [ selectedPhoto ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selecte-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (toString photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (SelectPhotoUrl url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


viewPhoto : String -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (SelectPhotoUrl url) ]
        [ text url ]


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ToggleExpanded path) ]
                [ text folder.name ]
    in
        if folder.expanded then
            let
                contents =
                    List.append
                        (List.indexedMap viewSubfolder folder.subfolders)
                        (List.map viewPhoto folder.photoUrls)
            in
                div [ class "folder expanded" ]
                    [ folderLabel
                    , div [ class "contents" ] contents
                    ]
        else
            div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type FolderPath
    = End
    | Subfolder Int FolderPath


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder
                    else
                        currentSubfolder
            in
                Folder { folder | subfolders = subfolders }
