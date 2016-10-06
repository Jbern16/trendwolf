port module TrendWolf exposing (..)


import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import String exposing (split, join)
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
    , ranking : String
    }

init =
   ( Model [] "trendWolf", Cmd.none )

-- UPDATE

type Msg
  = GetFonts String
  | Reset
  | Success (List Font)
  | Error Http.Error


port load : String -> Cmd msg

getFamilies : List Font -> String
getFamilies fonts =
  let
    families = List.map .family fonts
  in
    String.join (", ") families


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    GetFonts category ->
      ( ( Model model.fonts category ), requestFonts category )
    Reset ->
      ( Model [] "trendWolf", Cmd.none )
    Success fonts ->
      ( Model fonts model.category, load ( getFamilies fonts ) )
    Error _ ->
      ( Model model.fonts "Fonts Not Available", Cmd.none )

-- VIEW

header =
  div [ ]
   [ h1 [ class "headline"] [ text "trendWolf" ]
   , h2 [ class "sub-headline"] [text "Trending, Recent, and Most Popular Fonts From Google"]
   ]

homePage model =
  div [ class "landing" ]
    [ div [ class "endpoints" ]
      [ a [ onClick (GetFonts "trending"), href "#", class "button hpb" ] [ text "Check Out Trending" ]
      , a [ onClick (GetFonts "recent"),   href "#", class "button hpb" ] [ text "Check Out Recent" ]
      , a [ onClick (GetFonts "popular"),  href "#", class "button hpb" ] [ text "Check Out Popular"]
      ]
    ]

leadFontPage model =
  div [ class "lead" ]
  [ h1 [ class "popular" ] [ text model.category ]
    , a [ onClick (GetFonts "trending"),  href "#" ] [ text "Trending"]
    , a [ onClick (GetFonts "recent"), href "#" ] [ text "Recent"]
    , a [ onClick (GetFonts "popular"), href "#"] [ text "Popular"]
    , a [ onClick Reset, href "#", class "home" ] [ text "Home"]
  ]


fonts model =
  let
    showFont font =
      div [ class "font", style [ ( "font-family", font.family ) ] ]
      [ h2 [ ] [ text font.family  ]
      , p  [ ] [ text font.subsets ]
      , div [ class "ranking" ]
        [ h1 [ ] [ text font.ranking ] ]
      , div [ class "actions" ]
          [ a [ href font.file, class "button" ] [ text "Download!" ] ]
      , div [ class "inputs" ]
          [ h5 [ ] [ text "CDN:" ]
          , input [ value font.cdn ] [ ]
          ]
      ]
  in
    div [ class "all" ]
        ( List.map showFont model.fonts )


view : Model -> Html Msg
view model =
  if model.category == "trendWolf" then
    div [ class "welcome-page" ]
    [ header
    , homePage model
    ]
  else
    div [ ]
    [ leadFontPage model
    , fonts model
    ]


--SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

--HTTP

requestFonts : String -> Cmd Msg
requestFonts category =
  let
    url = "http://trendwolf.herokuapp.com/api/v1/fonts/" ++ category
  in
    Task.perform Error Success ( Http.get decodeFonts url)

decodeFont : Json.Decoder Font
decodeFont =
      Json.object5 Font
        ( Json.at ["table", "family"] Json.string )
        ( Json.at ["table", "file"] Json.string )
        ( Json.at ["table", "subsets"] Json.string )
        ( Json.at ["table", "cdn"] Json.string )
        ( Json.at ["table", "ranking"] Json.string )

decodeFonts : Json.Decoder (List Font)
decodeFonts =
  Json.list decodeFont
