import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Json
import Task

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEl

type alias Model =
  { fonts : List Font
  , category : String
  }

type alias Font =
    { family : String
    , file : String
    , subsets : String
    , cdn : String
    }

init =
   ( Model [] "trendWolf", Cmd.none )

-- UPDATE

type Msg
  = GetFonts String
  | Reset
  | Success (List Font)
  | Error Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    GetFonts category ->
      ( ( Model model.fonts category ), requestFonts category )
    Reset ->
      ( Model [] "trendWolf", Cmd.none )
    Success fonts ->
      ( Model fonts model.category, Cmd.none )
    Error _ ->
      ( Model model.fonts "failed", Cmd.none )

-- VIEW



homePage model =
  h1 [ class "headline"] (text "trendWolf")

view : Model -> Html Msg
view model =
  let
    showFont font =
      li []
        [ text ("Family" ++ font.family )
        , text ("Subset" ++ font.subsets)
        , text ("CDN" ++ font.cdn)
        , text ("File" ++ font.file)
        ]
  in
    div []
      [ h2 [ ] [ text model.category ]
      , a [ onClick Reset, href "#" ] [ text "Home"]
      , a [ onClick (GetFonts "trending"), href "#" ] [ text "Check Out Trending Fonts"]
      , a [ onClick (GetFonts "recent"),   href "#" ] [ text "Check Out Recent Fonts"]
      , a [ onClick (GetFonts "popular"),  href "#"] [ text "Check Out Popular Fonts"]
      , ul [] (List.map showFont model.fonts)
      ]

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

--HTTP

requestFonts : String -> Cmd Msg
requestFonts category =
  let
    url =
      "http://localhost:3000/api/v1/fonts/" ++ category
  in
    Task.perform Error Success ( Http.get decodeFonts url)

decodeFont : Json.Decoder Font
decodeFont =
      Json.object4 Font
        ( Json.at ["table", "family"] Json.string )
        ( Json.at ["table", "file"] Json.string )
        ( Json.at ["table", "subsets"] Json.string )
        ( Json.at ["table", "cdn"] Json.string )

decodeFonts : Json.Decoder (List Font)
decodeFonts =
  Json.list decodeFont
