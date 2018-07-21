module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import PhotoGroove exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import Test.Html.Query as Query
import Test.Html.Event as Event
import Test.Html.Selector exposing (text, tag, attribute)
import Html.Attributes as Attrs


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")


stateTransitions : Test
stateTransitions =
    describe "state transitions"
        [ fuzz string "SelectByUrl selects the given photo by url" <|
            \url ->
                PhotoGroove.initialModel
                    |> PhotoGroove.update (SelectByUrl url)
                    |> Tuple.first
                    |> .selectedUrl
                    |> Expect.equal (Just url)
        , fuzz (list string) "LoadPhotos selects the first photo" <|
            \urls ->
                let
                    photos =
                        List.map photoFromUrl urls
                in
                    PhotoGroove.initialModel
                        |> PhotoGroove.update (LoadPhotos (Ok photos))
                        |> Tuple.first
                        |> .selectedUrl
                        |> Expect.equal (List.head urls)
        ]


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos" <|
        \_ ->
            PhotoGroove.initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


allThumbnailsRendered : Test
allThumbnailsRendered =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
                { initialModel
                    | photos = List.map photoFromUrl urls
                }
                    |> PhotoGroove.view
                    |> Query.fromHtml
                    |> Expect.all thumbnailChecks


clickToSelect : Test
clickToSelect =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToClick urlsAfter ->
            let
                url =
                    urlToClick ++ ".jpeg"

                photos =
                    urlsBefore
                        ++ url
                        :: urlsAfter
                        |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
                { initialModel | photos = photos }
                    |> PhotoGroove.view
                    |> Query.fromHtml
                    |> Query.find [ tag "img", (attribute <| Attrs.src srcToClick) ]
                    |> Event.simulate Event.click
                    |> Event.expect (SelectByUrl url)


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", (attribute <| Attrs.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> toString num ++ ".png")
