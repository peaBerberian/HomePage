port module HomePage exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json


type alias SearchEngine =
  { name : String
  , url : String
  , searchPlaceholder : String
  , smallSrc : String
  , searchUrl : String
  }

defaultEngine =
  { name = "Google"
  , url = "http://www.google.com"
  , searchPlaceholder = "Search on Google"
  , smallSrc = "./assets/img/google-mini.png"
  , searchUrl = "http://www.google.com/search?q="
  }


searchEngines : List SearchEngine
searchEngines =
  [ { name = "Google"
    , url = "http://www.google.com"
    , searchPlaceholder = "Search on Google"
    , smallSrc = "./assets/img/google-mini.png"
    , searchUrl = "http://www.google.com/search?q="
    }
  , { name = "Bing"
    , url = "http://www.bing.com"
    , searchPlaceholder = "Search on Bing"
    , smallSrc = "./assets/img/bing-mini.png"
    , searchUrl = "http://www.bing.com/search?q="
    }
  , { name = "DuckDuckGo"
    , url = "http://www.duckduckgo.com"
    , searchPlaceholder = "Search on DuckDuckGo"
    , smallSrc = "./assets/img/duckduckgo-mini.png"
    , searchUrl = "http://www.duckduckgo.com?q="
    }
  , { name = "Pitchfork"
    , url = "http://pitchfork.com"
    , searchPlaceholder = "Search on pitchfork"
    , smallSrc = "./assets/img/pitchfork-mini.png"
    , searchUrl = "http://pitchfork.com/search/?query="
    }
  , { name = "Last.fm"
    , url = "http://www.last.fm"
    , searchPlaceholder = "Search on last.fm"
    , smallSrc = "./assets/img/lastfm-mini.png"
    , searchUrl = "http://www.last.fm/search?q="
    }
  , { name = "AOTY"
    , url = "http://www.albumoftheyear.org"
    , searchPlaceholder = "Search on albumoftheyear"
    , smallSrc = "./assets/img/aoty-mini.png"
    , searchUrl = "http://www.albumoftheyear.org/search.php?q="
    }
  ]


-- update window.location TODO find way to do it in elm
port location : String -> Cmd msg


type alias Model =
  { currentSearch : String
  , currentEngine : SearchEngine
  }


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg
  = NoOp
  | ChangeSearch String
  | ChangeSearchEngine SearchEngine
  | LaunchSearch


defaultModel : Model
defaultModel =
  { currentSearch = ""
  , currentEngine = Maybe.withDefault defaultEngine (List.head searchEngines)
  }


init : (Model, Cmd Msg)
init =
    (defaultModel, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    ChangeSearch str ->
      ({ model | currentSearch = str }, Cmd.none)

    ChangeSearchEngine engine ->
      (
        { model
        | currentEngine = engine
        , currentSearch = ""
        }
      , Cmd.none
      )

    LaunchSearch ->
      if model.currentSearch == "" then (model, Cmd.none) else
        let
          url = getSearchUrl model.currentEngine model.currentSearch
        in
          (model, location url)


getSearchUrl : SearchEngine -> String -> String
getSearchUrl engine text =
  engine.searchUrl ++ text


-- Declare onEnter event catcher
onEnter : Msg -> Attribute Msg
onEnter msg =
  let
      tagger code =
        if code == 13 then msg else NoOp
  in
      on "keydown" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
  div [class "main"]
    [ searchZone model
    ]


searchZone : Model -> Html Msg
searchZone model =
  div [class "search-zone"]
    [ engineInput model
    , linefeed
    , div [class "logos"] getEngineImageLinks
    ]


getEngineImageLinks : List (Html Msg)
getEngineImageLinks =
  List.map engineImageLink searchEngines


engineImageLink : SearchEngine -> Html Msg
engineImageLink engine =
  img
      [ class "engine-logo"
      , src engine.smallSrc
      , onClick (ChangeSearchEngine engine)
      ] []


engineInput : Model -> Html Msg
engineInput model =
  input
      [ placeholder model.currentEngine.searchPlaceholder
      , value model.currentSearch
      , class "engine-input"
      , onInput ChangeSearch
      , onEnter LaunchSearch
      ] []


linefeed : Html Msg
linefeed =
  br [] []
